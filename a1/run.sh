#! /bin/bash
rm ./t
cc -m64 -o t t.s runtime.c
./t
