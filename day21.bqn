#start ← 4‿8
start ← 3‿10

scores ← +` {𝕩+10×𝕩=0} 10| start +` (+´)⎉1 200‿2‿3 ⥊ 1+↕100
end ← ⊑ / (»⊸<∨`) «⥊scores≥1000
loser ←  end ⊑ ⥊scores
roll ← 3×2+ end
•Show loser × roll

Move ← { ⟨roll,player,⟨pos,score⟩⟩:
  pos2 ← {1+10|roll+𝕩-1}⌾(player⊸⊑) pos
  ⟨pos2, score + pos2 × player=↕≠pos2⟩
}
f ← 3↓<˘⍉(↕∘≠ ≍ ⊣) ≠¨⊔+˝⍉>⥊1+↕3⥊3
Play ← {
 ⟨player,⟨pos,score⟩⟩: ∨´score ≥ 21 ? score ≥ 21 ;
 ⟨player,st⟩: +´ { ⟨val,frq⟩: frq × Play (¬player)‿(Move val‿player‿st) }¨ f
}
•Show ⌈´ Play 0‿⟨start,⟨0,0⟩⟩
