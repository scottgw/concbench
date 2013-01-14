#!/bin/bash

CP=bin/:${AKKA_HOME}/lib/akka/akka-actor-2.0.5.jar:${AKKA_HOME}/lib/akka/config-0.3.1.jar

scala -cp ${CP} ch.ethz.se.concbench.anneal.Main $1
