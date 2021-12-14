str←•Import"bqn-libs/strings.bqn"
dots‿folds ← str.lf‿str.lf str.Split •FChars"day13"
dots ↩ {•BQN¨ ',' str.Split 𝕩}¨ str.lf str.Split dots
folds ↩ { { ⟨a,b⟩: a⋈•BQN b } '=' str.Split ¯1 ⊑ ' ' str.Split 𝕩}¨ str.lf str.Split ¯1↓folds

d← 1¨⌾(dots⊸⊑) 0⥊˜1+⌈´dots
Fold ← { ⟨"x",n⟩𝕊d: (n↑d) ∨ ⌽n↑(n+1)↓d ; ⟨"y",n⟩𝕊d: "x"‿n Fold⌾⍉ d }
•Show +´⥊(⊑ folds) Fold d
•Show ⍉" #" ⊏˜ d Fold´ ⌽folds
