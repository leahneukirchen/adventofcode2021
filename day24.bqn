# port of https://github.com/cettt/Advent_of_Code2021/blob/master/day_24.R
str←•Import"bqn-libs/strings.bqn"
Diff ← (¬∘∊/⊣)

d←{•BQN⊑⌽" " str.Split 𝕩}¨ 4‿5‿15 ⊏⎉1 14‿∘⥊•FLines"day24"

x1←eqidx←/9≥1⊏˘d
x2←⟨⟩
x1minmax←⟨⟩
x2minmax←⟨⟩

{
  x2 ∾↩ newmatch ← ⌈´ (↕𝕩) Diff x1∾x2
  b ← (newmatch‿2⊑d) + 𝕩‿1⊑d
  minmax ← (0 (<⋈>) b) + (⌊´⋈⌈´) / (>⟜0 ∧ <⟜10) b+↕9
  x2minmax ∾↩ <minmax
  x1minmax ∾↩ <minmax+b
}¨eqidx

res←(x1∾˘>x1minmax)∾(x2∾˘>x2minmax)

•Show '0'+⌽⍉1↓˘(⍋0⊏˘res)⊏res
