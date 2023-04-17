#! /usr/bin/env python
from subprocess import check_output

def get_pass(account: str) -> str:
    return check_output("pass email/" + account, shell=True).splitlines()[0]
