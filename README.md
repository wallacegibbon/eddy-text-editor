## Introduction

A modal text editor (like vi) with built-in T9 input method. You need only 3 fingers to write programs!

(Still under developing...)


## T9 Input Method

```
>>>     ABC     DEF

GHI     JKL     MNO

PQRS    TUV    WXYZ


>>> : next option
```


## Movement

```
<<       ^       >>

<        v        >

<<<      F      >>>        B


<<  : move one word left
>>  : move one word right
<<< : move to the start of the line
>>> : move to the end of the line
F   : forward page
B   : backward page
```


## Map tables

MAP 1

```
1        2        3

4        5        6

7        8        9        0
```

MAP 2

```
[        "        ]

(        '        )

{        .        }        ,
```


MAP 3

```
\        |        /

<        =        >

?        #        :        ;
```

MAP 4

```
+        *        -

@        %        &

!        ~        ^        _
```

MAP 5

```
$        `
```

## Commands

In command mode (just like normal mode in vi), keystrokes are connected to commands.

e.g.

```
77 => cut
78 => copy
79 => paste

44 => save
55 => quit
45 => save_and_quit
```

