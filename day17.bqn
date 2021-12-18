lx‿ux←209‿238 ⋄ ly‿uy←¯86‿¯59

# •Show ÷⟜2 (⊣ × -⟜1) |⊑y
•Show +´↕|ly

Check ← { ⟨⟨x,y⟩,⟨vx,vy⟩⟩: { (lx≤x)∧(x≤⊑ux)∧(ly≤y)∧(y≤uy) ? 1 ;
                             y < ly ? 0 ;
                             Check ⟨⟨x+vx, y+vy⟩, ⟨vx-×vx, vy-1⟩⟩ } }

•Show +´ ⥊ {Check ⟨0,0⟩‿𝕩}¨ (<1⋈ly)+↕⟨ux,2×|ly⟩
