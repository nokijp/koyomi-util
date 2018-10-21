# DateUtil

A command line tool to display various information about Japanese calendar, such as Japanese public holidays and Rokuyo.


## Build and Install

```bash
$ git clone https://github.com/nokijp/date-util.git
$ cd date-util
$ stack install 
```

[Stack](https://www.haskellstack.org/) is required to build DateUtil.


## Usage

### Checking if today is a holiday

The sub-command `isholiday` returns an exit code

```
$ dateutil isholiday
$ echo $?
0
```

### Show the type of holiday 2000/03/20 is a holiday
