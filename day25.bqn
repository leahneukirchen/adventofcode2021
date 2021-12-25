_fixcnt ← { 𝕩 ≡ 𝔽 𝕩 ? 𝕨 ; (𝕨+1) 𝕊 𝔽 𝕩 }

d←>•FLines "day25s"

•Show 1 { 𝕊 ⟨east, south⟩:
  east ↩ ((¯1⌽˘east)∧¬south∨east) ∨ 1⌽˘(¯1⌽˘east)∧south∨east
  south ↩ ((¯1⌽south)∧¬south∨east) ∨ 1⌽(¯1⌽south)∧south∨east
  east‿south
} _fixcnt (=⟜'>' ⋈ =⟜'v') d

# 601
