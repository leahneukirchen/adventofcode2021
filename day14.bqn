str←•Import"bqn-libs/strings.bqn"
template‿rules ← (⊑⋈2⊸↓) str.lf str.Split ¯1↓ •FChars"day14"
rules ↩ >(" -> " ⊸ str.Split)¨ rules

pairs ← ⊏˘rules
letters ← ⊑¨pairs

f ← +´˘ pairs ≡⌜ <˘ 2↕template
t ← ¬ (¯1 ↑ template) ⊒ letters

m ← ((<˘⊑¨rules) ≡⌜ pairs) + ((<˘⊑∘⌽¨⌽˘rules) ≡⌜ pairs)

•Show (⌈´-⌊´) +´¨ (⊐ letters) ⊔ t + m⊸(+˝∘×)⍟10 f
•Show (⌈´-⌊´) +´¨ (⊐ letters) ⊔ t + m⊸(+˝∘×)⍟40 f
