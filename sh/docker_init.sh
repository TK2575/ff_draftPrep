!#/bin/bash

docker run --name tk_selenium -d \
-p 4445:4444 -p 5901:5900 \
--memory 4096mb --shm-size 4g \
selenium/standalone-firefox-debug:latest

docker run --name tk_rstudio \
-e PASSWORD=password -e ROOT=TRUE \
-d -v {$PWD}:/var/repos \
-p 8787:8787 rocker/tidyverse
