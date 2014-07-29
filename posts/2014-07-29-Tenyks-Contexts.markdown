---
title: Conversations With Tenyks
date: 2014-07-29
---

Tenyks is a service-oriented IRC bot. It's job is relaying messages between IRC
and services. It uses JSON as the message package (speaking of which, I'm
looking at msgpack and ZeroMQ) and sends the message over Redis pub-sub.

The idea of conversation with a bot is a common one. But how do you pull it off
with a service-oriented architecture? This post intends to be a brain dump of
just that. Here are some ideas I've had over the past couple months of thinking
about it.
