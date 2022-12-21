#!/bin/bash
FILENAME=$1  
./signal.native -l $FILENAME.sgl > $FILENAME.out
lli $FILENAME.out

