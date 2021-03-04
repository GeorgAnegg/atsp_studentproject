#!/bin/bash
tar -xvf ALL_atsp.tar
gunzip *.gz
for f in *.atsp; do python CSVconversion.py $f; done
rm *.atsp
