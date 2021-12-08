d← •BQN '⟨'⊸∾ ∾⟜'⟩' •FChars"day07"

# Brute force O(n^2)
•Out ⌊´+˝|d -⌜ ↕≠d
•Out ⌊´+˝(÷⟜2×1⊸+)|d-⌜↕≠d

# More efficient
•Out +´|d-((≠d)÷2) ⊑ ∧d
•Out ⌊´+´˘(÷⟜2×1⊸+)|d -⌜˜ (⌊≍⌈) (+´ ÷ ≠)d
