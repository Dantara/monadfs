#!/bin/bash

docker build --tag=krokodilchk/dfs-namenode -f docker/Dockerfile-namenode ..
docker build --tag=krokodilchk/dfs-storage -f docker/Dockerfile-storage ..
docker build --tag=krokodilchk/dfs-client -f docker/Dockerfile-client ..
