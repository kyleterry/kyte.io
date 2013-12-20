---
title: Installing Arch Linux on UEFI With Luks and LVM
---

I've been using full disk encryption for quite some time. I almost refuse to own
a computer without it. It is an easy way to tack on an extra layer of security.

I recently got a motherboard that uses UEFI and I had to relearn everything
about booting [Arch Linux](https://www.archlinux.org/). Finding how to do this
in a clean, concise and understandable way is nearly impossible.

Here, I plan to layout the basic steps to get a base install up and running.

<!--more-->

These steps assume your setup is similar to mine. In my instance, I installed
Arch Linux without dual booting another operating system on a 120 GB SSD. I also
assume that you have already [created live
installer](https://wiki.archlinux.org/index.php/USB_Flash_Installation_Media)
and are already booted into the Arch Linux install. If you have data on the
drive, you should make a backup now; we are going to completely destroy every
partition on the drive and format it.

### [Partitioning](#partitioning)

We will be using a tool called `gdisk` to partition the drive. I created `sda1`
as 100M ext2 `/boot`, `sda2` as a 400M FAT32 EFI partition, and `sda3` as a
111.3G LUKS partition.

```
# gdisk
Command (? for help):
```

### [Mounting](#mounting)

### [Installing](#installing)
