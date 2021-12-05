str←•Import"bqn-libs/strings.bqn"

draws‿cards ← (⊑⋈1⊸↓) str.lf‿str.lf str.Split •FChars"day04"
draws ↩ > •BQN¨ ',' str.Split draws
cards ↩ >> {{•BQN¨ (⊢ /˜ (1⌊≠)¨) ' '⊸str.Split 𝕩}¨ str.lf⊸str.Split 𝕩}¨ ¯1↓cards

Cross ← { ¯1+(1+𝕩)×𝕩≠𝕨 }
Won ← { ∨´1=(×˝-𝕩)∾(×˝⍉-𝕩) }

winners ← {⊐⟜1 Won¨ (<𝕩) Cross˜` draws}˘ cards

•Out (⌊´winners)⊑draws×+´+˝0⌈((⊑⍋winners)⊏cards) Cross´ (1+⌊´winners)↑draws
•Out (⌈´winners)⊑draws×+´+˝0⌈((⊑⍒winners)⊏cards) Cross´ (1+⌈´winners)↑draws
