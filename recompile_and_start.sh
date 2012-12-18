#!/bin/bash

echo compiling...

erlc -o ebin/ src/*.erl

echo starting...

erl -pa ebin 

