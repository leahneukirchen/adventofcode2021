str←•Import"bqn-libs/strings.bqn"
Dec←(10⊸×+⊣)´∘⌽
d ← 'a'-˜" "⊸str.Split¨¨ " | "⊸str.Split¨ •FLines"day08"

•Out +´ 2‿3‿4‿7 ∊˜ ≠¨∾1⊑¨d

c ← 'a'-˜"abcefg"‿"cf"‿"acdeg"‿"acdfg"‿"bdcf"‿"abdfg"‿"abdefg"‿"acf"‿"abcdefg"‿"abcdfg"
P2 ← { ∧∘⊏⟜(+˝(∾𝕨)=⌜↕7)¨𝕩 }
•Out +´Dec¨((P2˜ c) ⊐ P2´)¨d
