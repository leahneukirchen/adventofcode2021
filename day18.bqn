d ← {•BQN ("⟨⟩"∾𝕩) ⊏˜ ("[]"∾𝕩) ⊐ 𝕩}¨•FLines"day18"

Split ← { 𝕊 ⟨l,r⟩: { 𝕊 @: { 𝕊 @: @ ; 𝕊 r2: l‿r2 } Split r ;
                     𝕊 l2: l2‿r } Split l ;
          𝕊 n: n ≥ 10 ? (⌊⋈⌈) n÷2 ;
          @ }

Addlm ← { n 𝕊 ⟨l,r⟩: ⟨n Addlm l, r⟩ ; n 𝕊 m : n + m }
Addrm ← { n 𝕊 ⟨l,r⟩: ⟨l, n Addrm r⟩ ; n 𝕊 m : n + m }

Explode ← {
  4 𝕊 ⟨l,r⟩: 0‿l‿r ;
  d 𝕊 ⟨l,r⟩: { 𝕊 @: { 𝕊 @: @ ;
                       𝕊 ⟨r2,ln,rn⟩: ⟨ln Addrm l,r2⟩‿0‿rn } (d+1) Explode r ;
               𝕊 ⟨l2,ln,rn⟩: ⟨l2,rn Addlm r⟩‿ln‿0 } (d+1) Explode l ;
  @
}

Reduce ← { 𝕊 t: { 𝕊 @: { 𝕊 @: t ; 𝕊 t3: Reduce t3 } Split t ;
                  𝕊 t2: Reduce t2 } ⊑ 0 Explode t }

Add ← Reduce∘⋈

Mag ← { 𝕊 ⟨l,r⟩: (3×Mag l)+(2×Mag r) ; 𝕊 n: n }

•Show Mag Add˜´ ⌽ d
•Show ⌈´⥊Mag∘Add⌜˜ d
