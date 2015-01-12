# subleq-mips
Implementatingf instruction of MIPS with subleq

# Architecture
## Registers
Each registers is mapped to the memory.

| Address | Meaning |
|---------|---------|
| R0-R15 | General registers |
| PC | |
| hi, lo | for multiplication |
| trap | interruption |

## Aliases
There are a few aliases.

| Address | Comment |
|---------|---------|
| Rd, Imm | |
| Rs | |
| Rt | |
| Mem | the location whither the indirect reference points |

## Special Registers
For internal use.
It allows to modify, but it should be resumed at the end of the subroutine.

| Address | Comment |
|---------|---------|
| inc | stores (-1) |
| dec | stores (+1) |
| Z | stores 0 |
| Mem | the location whither the indirect reference points |

# Format
Format is as shown.

```
@add Rd,Rs,Rt
Rd; // clear Rd
Rs Z; // Z <- -Rs, it assumes M[Z] = 0
Rt Z; // Z <- Z - Rt = -(Rs + Rt)
Z Rd;
Z;
```
