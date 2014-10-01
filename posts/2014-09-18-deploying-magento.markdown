---
title: Deploying Magento with Bash
date: 2014-09-18
bg_image: /images/9577857867_78b48fcdc1_k.jpg
---

There are so many ways to deploy code. It's actually pretty obnoxious how many
"deploy" systems there are. I've decided to just use bash. That's right, good
ol' shell scripting.

I don't know much about Magento on the application side. But I definitely know
how to handle Magento on the systems side.

Since I assist in running a large e-commerce site with dozens upon dozens of
servers, I use Chef to handle configuration management. I wrote my web server
recipe around the Git resource so I can push a new tag up to Github and run
chef-client to deploy that tagged code on the web nodes.

Utility nodes run cron jobs and other tasks that you don't really want running
on your web heads, but they still have php and a copy of the code base. So
utility nodes need to be deployed too.

Along with deploying code, you need to run migrations, flush cache, disable
monitoring temporarily, and put up the maintenance page. All these steps need to
be glued together and that's where Bash comes in.

Lets take a look at some of the steps you will need to write in bash. Out usage
will look like this:

```
Usage: deploy_magento.sh -e ENVIRONMENT -t TAG
    ENVIRONMENTS
        production
        staging
        development

    OPTIONS
        -e ENVIRONMENT
            Sets the environment to deploy against
        -t TAG
            Tag or git hash to deploy
```


First we will except some options from the command line:


```bash
#!/usr/bin/env bash

set -u
set -e

function init {
    # prepare options
    while getopts "he:t:m:" option
    do
        case ${option} in
        h)
            usage
            exit 0
            ;;
        e)
            ENVIRONMENT=${OPTARG}
            ;;
        t)
            TAG=${OPTARG}
            ;;
        esac
    done

    if [[ -z ${ENVIRONMENT} ]]; then
        log "FATAL: environment must be set"
        usage
        exit 1
    fi

    if [[ -z ${TAG} ]]; then
        log "FATAL: tag must be set"
        usage
        exit 1
    fi

    LOG_FILE_DIR=/var/log/deploy
    LOG_FILE=${LOG_FILE_DIR}/deploy-${ENVIRONMENT}.log
}
```

`init` will parse arguments with setup two variables: `ENVIRONMENT` and `TAG`.
`ENVIRONMENT` would be something like "production", "staging", or "development".
`TAG` would be something like "1.9.2" or even the hash of a git commit.

Now we have some initial setup. Lets setup a common logging function. This is
important because it will keep you from littering your script with echo's and
printf's that make it hard to read.

```bash
function log {
    local message=${1}

    if [[ -z ${message} ]]; then
        return
    fi

    echo "[$(date)] ${message}" | tee -a ${LOG_FILE}
}
```

Okay, now that we can log, lets take a look at building out setup for our
environment:

```bash
function setup_environment {
    case ${ENVIRONMENT} in
    production)
        CHEF_UTILITY_SEARCH="roles:utility AND chef_environment:production"
        CHEF_WEB_SEARCH="roles:web AND chef_environment:production"
        DISABLE_MONITORING=true
        ;;
    staging)
        CHEF_UTILITY_SEARCH="roles:utility AND chef_environment:staging"
        CHEF_WEB_SEARCH="roles:web AND chef_environment:staging"
        DISABLE_MONITORING=false
        ;;
    development)
        CHEF_UTILITY_SEARCH="roles:utility AND chef_environment:development"
        CHEF_WEB_SEARCH="roles:web AND chef_environment:development"
        DISABLE_MONITORING=false
        ;;
    esac
}
```

This is pretty simple. It's a case that setups up variables based on the
ENVIRONMENT. `CHEF_UTILITY_SEARCH` is a string you can pass to chef search to
get utility nodes that need to be deployed to. The best use of this is by the
`knife` tool. For instance, you can run chef on all those boxed by running
`knife ssh "${CHEF_UTILITY_SEARCH}" "sudo chef-client --once"`. You'll see this
in use in a bit

Now we need to setup a function that will ask us some questions in case we
accidentally get a bit trigger happy with that up arrow + enter key combination:

```bash
function confirmations {
    while true; do
        read -p "You are about to deploy to ${ENVIRONMENT}. Are you sure? [no] " answer
        case ${answer} in
            yes|Yes|YES)
                break
                ;;
            *)
                echo "Aborting."
                exit 1
                ;;
        esac
    done
    answer=
    while true; do
        read -p "Does your deploy contain migrations or anything requiring downtime? (yes or no) " answer
        case ${answer} in
            yes|Yes|YES)
                MAINTENANCE=true
                MAINTENANCE_FLAG="on"
                break
                ;;
            no|No|NO)
                MAINTENANCE=false
                MAINTENANCE_FLAG="off"
                break
                ;;
            *)
                echo "Please answer yes or no"
                ;;
        esac
    done
}
```

This is also pretty simple. It just asks the user for some yes or no input.
First, it will ask you if you are sure. Then it will ask if you have migrations
that require downtime. If you don't need to run migrations, chances are you can
just leave the site up and deploy the code to the nodes without taking the site
down. Otherwise, the maintenance page will be put up.

Okay, now lets make some functions for toggling maintenance and monitoring.

```bash
function toggle_maintenance_page {
    local state=${1}

    log "Toggling maintenance mode to ${state}"
    if [ "${state}" == "on" ]; then
        local action_command='touch'
        local action_description='Setting'
    elif [ "${state}" == "off" ]; then
        local action_command='rm -f'
        local action_description='Removing'
    else
        log "FATAL: Invalid state \"${state}\" for toggle_maintenance_page"
        exit 1
    fi

    if [ ! -z "$action_command" ]; then
        log "${action_description} maintenance flags on web nodes"
        knife ssh '${CHEF_WEB_SEARCH}' '${action_command} ${DOCROOT}/maintenance.flag' >> ${LOG_FILE} 2>&1
    fi
}

function toggle_monitoring {
    local state=${1}

    log "Toggling monitoring for maintenance mode to ${state}"
    if [ "${state}" == "on" ]; then
        curl -qs https://monitoring.example.com/api/enable
    elif [ "${state}" == "off" ]; then
        curl -qs https://monitoring.example.com/api/disable
    else
        log "FATAL: Invalid state \"${state}\" for toggle_maintenance_page"
        exit 1
    fi
}
```

These two are also pretty self explanatory. The `curl` commands are a good way
to access an API for our monitoring system to toggle it's state.

Now we are going to work on the deploy function. This is the function that
actually runs the deploy commands.

```bash
function do_deploy {
    if ${MAINTENANCE}; then
        if ${DISABLE_MONITORING}; then
            toggle_monitoring "off"
        fi
        toggle_maintenance_page "on"
    fi

    if [[ -n ${CHEF_UTIL_SEARCH} ]]; then
        log "Deploying utility"
        knife ssh ${CHEF_UTIL_SEARCH} "echo ${TAG} > /home/deploy/DEPLOY_TAG; sudo chef-client --once" >> ${LOG_FILE} 2>&1
    fi
    log "Deploying code to web nodes"
    knife ssh 'echo ${TAG} > /home/deploy/DEPLOY_TAG; ${CHEF_WEB_SEARCH}' 'sudo chef-client --once' >> ${LOG_FILE} 2>&1

    if ${MAINTENANCE}; then
        if ${DISABLE_MONITORING}; then
            toggle_new_relic_monitoring "on"
        fi
        toggle_maintenance_page "off"
    fi
}
```

Yep, that's really all there is to that function. It's pretty simple. Now we
must call everything we've made this far.

```bash
# main
init ${@}
setup_environment
confirmations
log "Starting the deploy of ${TAG}"
DEPLOY_START=${SECONDS}
log "Deploying code..."
do_deploy
DEPLOY_FINISH=${SECONDS}

log "Action took $((${DEPLOY_FINISH} - ${DEPLOY_START})) seconds."
log "Deploy complete!"

exit 0
```

This is the gist of it. If you have a good configuration management system, it
can help make deploying code trivial. This post kind of assumes you have been
working on the shell and have done some minor devops. But really, anyone can
read this and understand what's going on. The point is to just use bash for
these things. It would have taken me a week to build out Capistrano or fabric. I
did a much more detailed version of this bash deploy script in about a day.

I hope this helps.
