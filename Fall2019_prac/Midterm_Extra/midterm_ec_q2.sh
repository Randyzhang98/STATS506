#!/bin/bash
#--------------------------------------------
# author：Sijun Zhang
# last change date: 09/21/2019 
#--------------------------------------------

grep -rn -E '(gather\(|spread\()' *.R*
