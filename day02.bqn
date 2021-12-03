d←•FLines"day02"
c←⊑¨d
n←'0'-˜¯1⊑¨d

pos←+´n×'f'=c
aim←+`n×1-˜"ufd"⊐c
depth←⊑⌽aim
•Out pos×depth

depth2←+´n×aim×'f'=c
•Out pos×depth2

{ # Golfed:
•Out (p←+´m←n×c=9)×⊑⌽a←+`(n←'0'-˜¯1⊑¨d)×1-˜4‿9‿6⊐c←≠¨d←•FLines"day02"
•Out p×+´a×m
}
