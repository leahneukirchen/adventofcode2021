d←'0'-˜•FLines"day03"
Dec←(2⊸×+⊣)´∘⌽

•Out (2×+´⍉d)(≤×○Dec>)≠d

_part2 ← { 1=≠𝕩 ? ⊑𝕩 ; (𝕨+1) 𝕊 𝕩/˜ =⟜(+´ 𝔽 ÷⟜2∘≠) 𝕨 ⊑¨ 𝕩 }
•Out (0 ≥_part2 d) ×○Dec (0 <_part2 d)
