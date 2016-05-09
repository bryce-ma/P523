#! /bin/bash

cc -m64 -o t t.s runtime.c
./t
