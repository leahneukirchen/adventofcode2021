str←•Import"bqn-libs/strings.bqn"
d ← {•BQN¨∾{"," str.Split 𝕩}¨ " -> " str.Split 𝕩}¨ •FLines"day05"

Line ← {s<⊸+(<×e-s)×↕1+⌈´|(e←2↓𝕩)-(s←2↑𝕩)}
•Out +´1=⊒∾Line¨d /˜ 0=×´˘(>d)-2⌽˘>d
•Out +´1=⊒∾Line¨d
