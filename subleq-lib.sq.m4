changequote(<,>)
// Here quote mark in not possible to use.
// I use 4%b1101 instead of 4'1101.
changequote`'

define(`PureSubleq',ifelse(ARCH,`subleq',`$1',`$1O'))
define(`RDec',`Min')

@@sltSub Rd,Rs,Rt, T0, AEnd
Rt T0; Rd;
Z Rs Lsnz;
Lsp:Z Rt Lf;
Lsptp:Rd Rd Lcomp;
Lsnz:Z Rt Lcomp;
Lsnztp:Rd Rd Lt;
Lcomp:Rs Rt;
$(@@jnzp Rt, Lf, Lf, Lt);
Lt:Inc Rd;
Lf:Rt; T0 Rt; T0 T0 AEnd;

@@add Ad,As,At, AEnd
As Z; // Z <- -As, it assumes Z = 0
At Z; // Z <- Z - At = -(As + At)
Ad; // clear Ad
Z Ad;
Z Z AEnd;

@@sllsubbody Rd, S2
S2 Rd; S2; Rd S2;

PureSubleq(`@@sllsub') Rd, Rt, Sa, S1, S2, Aend // Rd <- Rt << Sa
Rt S2; Rd;
$(@@jnzp Rt, L1, LFinish, L1);
L1:S2 Rd;
Sa S1; // S1 = -Sa
Loop:Inc S1 LBody;
LFinish:S2; S1 S1 Aend;
LBody:$(@@sllsubbody Rd, S2); Z Z Loop;

PureSubleq(`@@slli3sub') Rd, Rt, Sa, S1, S2, Aend // Rd <- Rt << (Sa << 3)
Rt S2; Rd;
$(@@jnzp Rt, L1, LFinish, L1);
L1:S2 Rd; // S2 == -Rd
Sa S1; // S1 = -Sa
Loop:Inc S1 LBody0;
LFinish:S2; S1 S1 Aend;
LBody0:$(@@sllsubbody Rd, S2);
LBody1:$(@@sllsubbody Rd, S2);
LBody2:$(@@sllsubbody Rd, S2);
LBody3:$(@@sllsubbody Rd, S2);
LBody4:$(@@sllsubbody Rd, S2);
LBody5:$(@@sllsubbody Rd, S2);
LBody6:$(@@sllsubbody Rd, S2);
LBody7:$(@@sllsubbody Rd, S2);
Z Z Loop;

PureSubleq(`@@slli4sub') Rd, Rt, Sa, S1, S2, Aend // Rd <- Rt << (Sa << 4)
Rt S2; Rd;
$(@@jnzp Rt, L1, LFinish, L1);
L1:S2 Rd; // S2 == -Rd
Sa S1; // S1 = -Sa
Loop:Inc S1 LBody0;
LFinish:S2; S1 S1 Aend;
LBody0:$(@@sllsubbody Rd, S2);
LBody1:$(@@sllsubbody Rd, S2);
LBody2:$(@@sllsubbody Rd, S2);
LBody3:$(@@sllsubbody Rd, S2);
LBody4:$(@@sllsubbody Rd, S2);
LBody5:$(@@sllsubbody Rd, S2);
LBody6:$(@@sllsubbody Rd, S2);
LBody7:$(@@sllsubbody Rd, S2);
LBody8:$(@@sllsubbody Rd, S2);
LBody9:$(@@sllsubbody Rd, S2);
LBody10:$(@@sllsubbody Rd, S2);
LBody11:$(@@sllsubbody Rd, S2);
LBody12:$(@@sllsubbody Rd, S2);
LBody13:$(@@sllsubbody Rd, S2);
LBody14:$(@@sllsubbody Rd, S2);
LBody15:$(@@sllsubbody Rd, S2);
Z Z Loop;

PureSubleq(`@@srlsub') Rd, Rt, Sa, S0, S1, Aend
$(@@jnzp Rt, LStart, LFinish2, LStart);
LStart:Rt S1; Rd;
Sa S0; CW Sa;
Loop:Inc Sa LBody;
LFinish:Sa; S0 Sa; S0; Rt; S1 Rt; S1 S1 Aend;
LBody:$(@@sl1d Rd, Rt, Loop);
LFinish2:Rd Rd Aend;

PureSubleq(`@@srli3sub') Rd, Rt, Sa, S0, S1, Aend
$(@@jnzp Rt, LStart, LFinish2, LStart);
LStart:Rt S1; Rd;
Sa S0; CW3 Sa;
Loop:Inc Sa LBody;
LFinish:Sa; S0 Sa; S0; Rt; S1 Rt; S1 S1 Aend;
LBody:$(@@sl8d Rd, Rt, Loop);
LFinish2:Rd Rd Aend;
CW3:(shift WordLength (- 3));

PureSubleq(`@@srli4sub') Rd, Rt, Sa, S0, S1, Aend
$(@@jnzp Rt, LStart, LFinish2, LStart);
LStart:Rt S1; Rd;
Sa S0; CW4 Sa;
Loop:Inc Sa LBody;
LFinish:Sa; S0 Sa; S0; Rt; S1 Rt; S1 S1 Aend;
LBody:$(@@sl16d Rd, Rt, Loop);
LFinish2:Rd Rd Aend;
CW4:(shift WordLength (- 4));

PureSubleq(`@@srasub') Rd, Rt, Sa, S0, S1, S2, Aend
Rt S1; Rd;
$(@@jnzp Rt, Ln, LFinish2, Lzp);
Ln:Dec Rd;
Lzp:Sa S0; CW Sa;
Loop:Inc Sa LBody;
LFinish:Sa; S0 Sa; S0; Rt; S1 Rt; S1 S1 Aend;
LBody:$(@@sl1d Rd, Rt, Loop);
LFinish2:S1 S1 Aend;

define(`subroutine_per_size',`
@@$1 Rd, Rt, Sa, $5, Aend
$(@@jnzp Rt, LStart, LFinish, LStart);
LStart:Rd; Sa Rd; Threshold Rd LBig;
$(@@$2 Rd, Rt, Sa, $5, Aend);
LBig:$(@@$3 Rd, Rt, Sa, $5, Aend);
LFinish:Rd Rd Aend;
Threshold:(- $4);
')

ifelse(ARCH,`subleqr',`
subroutine_per_size(`sllsub', `sllsubO', `sllsubBig', `22', `S1, S2')
subroutine_per_size(`srlsub', `srlsub1', `srlsubO', `21', `S1, S2')
subroutine_per_size(`srasub', `srasub1', `srasubO', `20', `S1, S2, S3')

subroutine_per_size(`slli3sub', `slli3subO', `slli3subBig', `10', `S1, S2')
subroutine_per_size(`slli4sub', `slli4subO', `slli4subBig', `10', `S1, S2')
subroutine_per_size(`srli3sub', `srli3sub1', `srli3subO', `21', `S1, S2')
subroutine_per_size(`srli4sub', `srli4sub1', `srli4subO', `21', `S1, S2')
subroutine_per_size(`srai3sub', `srai3sub1', `srai3subO', `20', `S1, S2, S3')
subroutine_per_size(`srai4sub', `srai4sub1', `srai4subO', `20', `S1, S2, S3')

@@sllsubBig Rd, Rt, Sa, S0, S1, Aend
Rt S1; Rd;
Sa S0; CW Sa;
Loop:Inc Sa LBody;
LFinish:Sa; S0 Sa; S0; Rt; S1 Rt; S1 S1 Aend;
LBody:$(@@srl1d Rt, Rd, Loop);

@@slli3subBig Rd, Rt, Sa, S0, S1, Aend
Rt S1; Rd;
Sa S0; CW3 Sa;
Loop:Inc Sa LBody;
LFinish:Sa; S0 Sa; S0; Rt; S1 Rt; S1 S1 Aend;
LBody:$(@@srl8d Rt, Rd, Loop);
CW3:(shift WordLength (- 3));

@@slli4subBig Rd, Rt, Sa, S0, S1, Aend
Rt S1; Rd;
Sa S0; CW4 Sa;
Loop:Inc Sa LBody;
LFinish:Sa; S0 Sa; S0; Rt; S1 Rt; S1 S1 Aend;
LBody:$(@@srl16d Rt, Rd, Loop);
CW4:(shift WordLength (- 4));

@@srlsubbody Rd, S2, Anext
S2 Rd (- L2); L2:S2; Rd S2 (- Anext);

@@srl1subbody Rd, S2, Anext
$(@@srlsubbody Rd, S2, Next);
Next:$(@@srlsubbody Rd, S2, Anext);

@@srli2subbody Rd, S2, Anext
$(@@srl1subbody Rd, S2, Next);
Next:$(@@srl1subbody Rd, S2, Anext);

@@srli3subbody Rd, S2, Anext
$(@@srli2subbody Rd, S2, Next);
Next:$(@@srli2subbody Rd, S2, Anext);

@@srli4subbody Rd, S2, Anext
$(@@srli3subbody Rd, S2, Next);
Next:$(@@srli3subbody Rd, S2, Anext);

@@srlsub1 Rd, Rt, Sa, S1, S2, Aend
Rt S2 (- L2); L2:Rd; L3:S2 Rd (- L4);
L4:Sa S1; // S1 = -Sa
Loop:Inc S1 LBody;
LFinish:S2; S1 S1 Aend;
// LBody:S2 Rd (- LB2); LB2:S2; Rd S2 (- LB4); LB4:Z Z Loop;
LBody:$(@@srlsubbody Rd, S2, LEnd); LEnd:Z Z Loop;

@@srli3sub1 Rd, Rt, Sa, S1, S2, Aend
Rt S2 (- L2); L2:Rd; L3:S2 Rd (- L4);
L4:Sa S1; // S1 = -Sa
Loop:Inc S1 LBody;
LFinish:S2; S1 S1 Aend;
// LBody:S2 Rd (- LB2); LB2:S2; Rd S2 (- LB4); LB4:Z Z Loop;
LBody:$(@@srli3subbody Rd, S2, LEnd); LEnd:Z Z Loop;

@@srli4sub1 Rd, Rt, Sa, S1, S2, Aend
Rt S2 (- L2); L2:Rd; L3:S2 Rd (- L4);
L4:Sa S1; // S1 = -Sa
Loop:Inc S1 LBody;
LFinish:S2; S1 S1 Aend;
// LBody:S2 Rd (- LB2); LB2:S2; Rd S2 (- LB4); LB4:Z Z Loop;
LBody:$(@@srli4subbody Rd, S2, LEnd); LEnd:Z Z Loop;

@@srasubbody Rd, S2, S3, Anext
S2 Rd (- L2); L2:S2; S3 Rd; Rd S2 (- Anext);

@@sra1subbody Rd, S2, Anext
$(@@srlsubbody Rd, S2, Next);
Next:$(@@srlsubbody Rd, S2, Anext);

@@sra2subbody Rd, S2, Anext
$(@@srl1subbody Rd, S2, Next);
Next:$(@@srl1subbody Rd, S2, Anext);

@@sra3subbody Rd, S2, Anext
$(@@srli2subbody Rd, S2, Next);
Next:$(@@srli2subbody Rd, S2, Anext);

@@sra4subbody Rd, S2, Anext
$(@@srli3subbody Rd, S2, Next);
Next:$(@@srli3subbody Rd, S2, Anext);

@@srasub1 Rd, Rt, Sa, S1, S2, S3, Aend
Rt S2 (- L2); L2:Rd; L3:S2 Rd (- L4);
L4:Sa S1; // S1 = -Sa
$(@@jnzp Rt, Ln, Loop, Loop);
Ln:Min S3;
Loop:Inc S1 LBody;
LFinish:S2; S3; S1 S1 Aend;
LBody:$(@@srasubbody Rd, S2, S3, LEnd); LEnd:Z Z Loop;

@@sra3sub1 Rd, Rt, Sa, S1, S2, S3, Aend
Rt S2 (- L2); L2:Rd; L3:S2 Rd (- L4);
L4:Sa S1; // S1 = -Sa
$(@@jnzp Rt, Ln, Loop, Loop);
Ln:Min S3;
Loop:Inc S1 LBody;
LFinish:S2; S3; S1 S1 Aend;
LBody:$(@@sra3subbody Rd, S2, S3, LEnd); LEnd:Z Z Loop;

@@sra4sub1 Rd, Rt, Sa, S1, S2, S3, Aend
Rt S2 (- L2); L2:Rd; L3:S2 Rd (- L4);
L4:Sa S1; // S1 = -Sa
$(@@jnzp Rt, Ln, Loop, Loop);
Ln:Min S3;
Loop:Inc S1 LBody;
LFinish:S2; S3; S1 S1 Aend;
LBody:$(@@srarsubbody Rd, S2, S3, LEnd); LEnd:Z Z Loop;

')

@@sl1d Ah, Al, Aend // {1%d_, Ah, Al} <- {Ah, Al, 1%d0}
$(@@sl1m Ah, L1); L1:Z Al Lzn;
Lp:$(@@sl1m Al, Aend);
Lzn:Inc Al Ln; Dec Al Lp;
Ln:Dec Al; Inc Ah; $(@@sl1m Al, Aend);

@@sl2d Ah, Al, Aend // {2%d_, Ah, Al} <- {Ah, Al, 2%d0}
Body0:$(@@sl1d Ah, Al, Body1);
Body1:$(@@sl1d Ah, Al, Aend);

@@sl4d Ah, Al, Aend // {4%d_, Ah, Al} <- {Ah, Al, 4%d0}
Body0:$(@@sl2d Ah, Al, Body1);
Body1:$(@@sl2d Ah, Al, Aend);

@@sl8d Ah, Al, Aend // {8%d_, Ah, Al} <- {Ah, Al, 8%d0}
Body0:$(@@sl4d Ah, Al, Body1);
Body1:$(@@sl4d Ah, Al, Aend);

@@sl16d Ah, Al, Aend // {16%d_, Ah, Al} <- {Ah, Al, 16%d0}
Body0:$(@@sl8d Ah, Al, Body1);
Body1:$(@@sl8d Ah, Al, Aend);

ifelse(ARCH,`subleqr',`
@@srl1d Ah, Al, Aend // {Ah, Al, 1%d_} <- {1%b0, Ah, Al}
$(@@srl1m Al, L1); L1:Z Ah (- Lzo);
Le:$(@@srl1m Ah, Aend);
Lzo:Inc Ah (- Lo); RDec Ah (- Le);
Lo:RDec Ah (- Lo2); Lo2:Inc Al (- Lo3); Lo3:$(@@srl1m Ah, Aend);

@@srl2d Ah, Al, Aend // {Ah, Al, 2%d_} <- {2%b0, Ah, Al}
Body0:$(@@srl1d Ah, Al, Body1);
Body1:$(@@srl1d Ah, Al, Aend);

@@srl4d Ah, Al, Aend // {Ah, Al, 4%d_} <- {4%b0, Ah, Al}
Body0:$(@@srl2d Ah, Al, Body1);
Body1:$(@@srl2d Ah, Al, Aend);

@@srl8d Ah, Al, Aend // {Ah, Al, 8%d_} <- {8%b0, Ah, Al}
Body0:$(@@srl4d Ah, Al, Body1);
Body1:$(@@srl4d Ah, Al, Aend);

@@srl16d Ah, Al, Aend // {Ah, Al, 16%d_} <- {16%b0, Ah, Al}
Body0:$(@@srl8d Ah, Al, Body1);
Body1:$(@@srl8d Ah, Al, Aend);

')

@@sl1dc Ad, Ah, Al, Aend // {Ad, Al} <- {(w-1)%b0, Al, 1%b0}; {1%_, Ah} <- {Ah, Ad[0]}
$(@@sl1m Ah, L1); L1:Ad;
Z Al Lzn;
Lp:$(@@sl1m Al, Aend);
Lzn:Inc Al Ln; Dec Al Lp;
Ln:Dec Al; Inc Ad; Inc Ah; $(@@sl1m Al, Aend);

@@sl1c Ad, Al, Aend // {Ad, Al} <- {(w-1)%b0, Al, 1%d0} ; no alias
L1:Ad;
$(@@sl1ca Ad, Al, Aend);

@@sl1ca Ad, Al, Aend // {A, Al} <- {(w-1)%b0, Al, 1%d0} ; Ad <- Ad + A ; alias ok ; end ok
Z Al Lzn;
Lp:$(@@sl1m Al, Aend);
Lzn:Inc Al Ln; Dec Al Lp;
// originally: Ln:Dec Al; Inc Ad; $(@@sl1m Al, Aend);
Ln:Dec Al; Al Z; Z Al; Inc Ad; Z Z Aend;

@@rl1m Ad, Aend
$(@@sl1ca Ad, Ad, Aend);

@@rl2m Ad, Aend
$(@@rl1m Ad, L1);
L1:$(@@rl1m Ad, Aend);

@@rl4m Ad, Aend
$(@@rl2m Ad, L1);
L1:$(@@rl2m Ad, Aend);

@@rl8m Ad, Aend
$(@@rl4m Ad, L1);
L1:$(@@rl4m Ad, Aend);

@@rl16m Ad, Aend
$(@@rl8m Ad, L1);
L1:$(@@rl8m Ad, Aend);

@@rl30m Ad, Aend
$(@@rl16m Ad, L1);
L1:$(@@rl8m Ad, L2);
L2:$(@@rl4m Ad, L3);
L3:$(@@rl2m Ad, Aend);

@@slrl1m As, Ar, S0, Aend // As <- {As[w-2:0], Ar[w-1]}  Ar <- {Ar[w-2:0], Ar[w-1]}
$(@@sl1ca S0, Ar, L1);
L1:Z S0 Lz;
Lp:Inc Ar; S0; Dec S0;
Lz:$(@@sl1m As, L2);
L2:S0 As; S0 S0 Aend;

@@slrl2m As, Ar, S0, Aend
$(@@slrl1m As, Ar, S0, L1);
L1:$(@@slrl1m As, Ar, S0, Aend);

@@slrl4m As, Ar, S0, Aend
$(@@slrl2m As, Ar, S0, L1);
L1:$(@@slrl2m As, Ar, S0, Aend);

@@slrl8m As, Ar, S0, Aend
$(@@slrl4m As, Ar, S0, L1);
L1:$(@@slrl4m As, Ar, S0, Aend);

@@slrl16m As, Ar, S0, Aend
$(@@slrl8m As, Ar, S0, L1);
L1:$(@@slrl8m As, Ar, S0, Aend);

ifelse(ARCH,`subleqr',`
@@srl1ca Ad, Al, Aend // {Al, A} <- {1%d0,  Al, (w-1)%b0} ; Ad <- Ad + A ; alias ok ; end ok
Z Al (- Lzo);
Le:$(@@srl1m Al, Aend);
Lzo:Inc Al (- Lo); RDec Al (- Le);
// originally: Lo:Dec Al; Inc Ad; $(@@sl1m Al, Aend);
Lo:RDec Al (- Lo2); Lo2:Al Z (- Lo3); Lo3:Z Al (- Lo4); Lo4:Inc Ad; Z Z Aend;

@@srl1cr Ad, Al, Aend // {Al, A} <- {1%d0,  Al, (w-1)%b0} ; Ad <- rev(rev(Ad) + rev(A)) ; alias ok ; end ok
Z Al (- Lzo);
Le:$(@@srl1m Al, Aend);
Lzo:Inc Al (- Lo); RDec Al (- Le);
// originally: Lo:Dec Al; Inc Ad; $(@@sl1m Al, Aend);
Lo:RDec Al (- Lo2); Lo2:Al Z (- Lo3); Lo3:Z Al (- Lo4); Lo4:Inc Ad (- Lo5); Lo5:Z Z Aend;

@@rr1m Ad, Aend
$(@@srl1cr Ad, Ad, Aend);

@@rr2m Ad, Aend
$(@@rr1m Ad, L1);
L1:$(@@rr1m Ad, Aend);

@@rr4m Ad, Aend
$(@@rr2m Ad, L1);
L1:$(@@rr2m Ad, Aend);

@@rr8m Ad, Aend
$(@@rr4m Ad, L1);
L1:$(@@rr4m Ad, Aend);

@@rr16m Ad, Aend
$(@@rr8m Ad, L1);
L1:$(@@rr8m Ad, Aend);

@@rrsl1m Ar, As, S0, Aend
$(@@srl1ca S0, Ar, L1);
L1:Z S0 Lz;
Lp:Inc Ar (- Lp1); Lp1:S0; Dec S0; // Here Dec w/ sub and RDec w/ rsub are equal.
Lz:$(@@srl1m As, L2);
L2:S0 As (- L3); L3:S0 S0 Aend;
')

@@sl1cj Al, Az, Ao // {Ad, Al} <- {(w-1)%b0, Al, 1%d0} ; if Ad == 1 then jump Ao else jump Az ; alias ok
Z Al Lzn;
Lp:$(@@sl1m Al, Az);
Lzn:Inc Al Ln; Dec Al Lp;
Ln:Dec Al; $(@@sl1m Al, Ao);

ifelse(ARCH,`subleqr',`
@@srl1 Ad, As, Aend // {Ad, 1%_} <- {1%b0, As} ; alias ok
As Z (- L2); L2:Ad; Z Ad (- L4); L4:Z Ad (- L5); L5:Z Z Aend;

@@srl1m A, Aend // {A, 1%d_} <- {1%d0, A} ; alias ok
A Z (- L2); L2:Z A (- L3); L3:Z Z Aend;

@@srl2m A, Aend // {A, 2%d_} <- {2%d0, A} ; alias ok
$(@@srl1m A, Next);
Next:$(@@srl1m A, Aend);

@@srl4m A, Aend // {A, 4%d_} <- {4%d0, A} ; alias ok
$(@@srl2m A, Next);
Next:$(@@srl2m A, Aend);

@@srl8m A, Aend // {A, 8%d_} <- {8%d0, A} ; alias ok
$(@@srl4m A, Next);
Next:$(@@srl4m A, Aend);

@@srl16m A, Aend // {A, 16%d_} <- {16%d0, A} ; alias ok
$(@@srl8m A, Next);
Next:$(@@srl8m A, Aend);
')

@@sl1 Ad, As, Aend // {1%d_, Ad} <- {As, 1%d0} ; alias ok ; end ok
As Z; Ad; Z Ad; Z Ad; Z Z Aend;

@@sl1m A, Aend // {1%d_, A} <- {A, 1%d0} ; alias ok ; end ok
A Z; Z A; Z Z Aend;

@@jnzp A, An, Az, Ap // goto An if A < 0, Az if A = 0, Ap if A > 0
Z A Lzn; Z Z Ap;
Lzn:Inc A Ln; Dec A Az;
Ln:Dec A An;

@@jznz A, Az, Anz // goto Az if A = 0, Anz if A != 0
Z A Lzn; Z Z Anz;
Lzn:Inc A Ln; Dec A Az;
Ln:Dec A Anz;

dnl @@jeq Ax, Ay, T, Ae, Ane // goto Ae if A == 0, Az if A = 0, Ap if A > 0, ON GOING
dnl Ay T; Ay Ax Lle;
dnl Lgt:T Ax; T T Ane
dnl Lle:
dnl Lzn:Inc A Ln; Dec A Az;
dnl Ln:Dec A An;
dnl $(@@jnzp Lne, Le, Lne);
dnl Le:T Ax; T T Ae;
dnl Lne:T Ax; T T Ae;

ifelse(ARCH,`subleqr',`
@@jezo A, Ae, Az, Ao // goto Az if zero, Ae if A is even, Ao if A is odd.
Z A (- Lzo); Z Z Ae;
Lzo:Inc A (- Lo); Lz:Min A (- Az);
Lo:Min A (- Ao);
')

@@jeo A, Ae, Ao // goto Ae if A is even, Ao if A is odd.
$(@@jezo A, Ae, Ae, Ao);

@@jltz A, Altz, Agez // goto Ae if A is even, Ao if A is odd.
A Z Lpz;
Ln:Z Z Altz;
Lpz:Z Z Agez;

@@addc Ah, Al, A, T, Aend
$(@@jnzp Al, Lln, Llp, Llp);
Lln:Inc T;
Llp:$(@@jnzp A, Lan, Lap, Lap);
Lan:Inc T;
Lap:A Z; Z Al; Z;
$(@@jnzp Al, Lxn, Lxp, Lxp);
Lxn:Dec T;
Lxp:Z T Ltnz;
Ltp:Inc Ah;
Ltnz:T T Aend;

@@multuDumbSub Hi, Lo, Rs, Rt, T0, T1, T2, T3, End
Lo; Hi; Rs T1;
Loop:Z Rs Lnz;
Lp:Dec Rs; $(@@addc Hi, Lo, Rt, T0, Loop);
Lnz:Inc Rs Ln; 
LFinish:T0; Rs; T1 Rs; T1 T1 End;
Ln:Dec Dec Rs; Z Z Lp;

@@multuDumbSub2 Hi, Lo, Rs, Rt, T0, T1, T2, T3, End
Lo; Hi; Rs T1;
Loop:$(@@jnzp Rs, LBody, LFinish, LBody);
LFinish:T0; Rs; T1 Rs; T1 T1 End;
LBody:Dec Rs; $(@@addc Hi, Lo, Rt, T0, Loop);

PureSubleq(`@@multuSub') Hi, Lo, Rs, Rt, T0, T1, T2, T3, End
Lo; Rs T1;
CW T0;
Loop:Inc T0 LBody;
LFinish:T0; Rs; T1 Rs; T1; T3 T3 End;
LBody:$(@@sl1d Hi, Lo, LBody2);
LBody2:$(@@sl1c T3, Rs, LBody3);
LBody3:Z T3 Loop;
$(@@addc Hi, Lo, Rt, T2, Loop);

ifelse(ARCH,`subleqr',`
@@multuSub Hi, Lo, Rs, Rt, TRth, T2, Ts, Tt, End
Hi; Lo; Rs Ts; Rt Tt;
// $(@@exch Rs, Rt, Ts, Tt, Loop); // for exchange
Loop:$(@@jezo Rs, LBodyE, LFinish, LBodyO);
LFinish:Ts Rs; Ts; Rt; Tt Rt; TRth; Tt Tt End;
LBodyO:$(@@addc Hi, Lo, Rt, T2, LBodyO1);
LBodyO1:TRth Z; Z Hi; Z;
LBodyE:$(@@sl1d TRth, Rt, LBodyE1);
LBodyE1:$(@@srl1m Rs, Loop);
')

@@multSub Hi, Lo, Rs, Rt, T0, T1, T2, T3, T4, T5, T6, Aend
Rs T5; Rt T6;
Inc T4; $(@@jnzp Rs, Lsn, Clear, Lsp);
Lsn:Inc T4; $(@@neg Rs, Rs, T0, Lsp);
Lsp:$(@@jnzp Rt, Ltn, Clear, Ltp);
Ltn:Inc T4; $(@@neg Rt, Rt, T0, Lsp);
Ltp:$(@@multuSub Hi, Lo, Rs, Rt, T0, T1, T2, T3, Lneg);
Lneg:Dec T4 Finish;
$(@@inv Hi, Hi, T0, LinvL);
LinvL:$(@@inv Lo, Lo, T0, Linc);
Linc:$(@@addc Hi, Lo, Dec, T0, Lneg);
Clear:Hi; Lo Lo;
Finish:Rs; T5 Rs; T5; Rt; T6 Rt; T6; T4 T4 Aend;

@@multDSub Hi, Lo, Rs, Rt, T0, T1, T2, T3, T4, Aend
Inc T4; $(@@jnzp Rs, Lsn, Clear, Lsp);
Lsn:Inc T4; $(@@neg Rs, Rs, T0, Lsp);
Lsp:$(@@jnzp Rt, Ltn, Clear, Ltp);
Ltn:Inc T4; $(@@neg Rt, Rt, T0, Lsp);
Ltp:$(@@multuSub Hi, Lo, Rs, Rt, T0, T1, T2, T3, Lneg);
Lneg:Dec T4 Finish;
$(@@inv Hi, Hi, T0, LinvL);
LinvL:$(@@inv Lo, Lo, T0, Linc);
Linc:$(@@addc Hi, Lo, Dec, T0, Lneg);
Clear:Hi; Lo Lo;
Finish:T4 T4 Aend;

@@exch As, At, Ts, Tt, Aend // [cond: End must be the next of this subroutine; use: Ts, Tt, invariant: Ts = -As, Tt = -At] As <- min(-Ts, -Tt), At <- max(-Ts, -Tt) ;
Z As Lzns;
Lps:Z At Aend; // go to Aend if As > 0 and At <= 0
Lnnpp:As At Lnonswap; $(@@swapNeg As, At, Ts, Tt, Aend);
Lzns:Z As Lnnpp;
Lznspt:$(@@swapNeg As, At, Ts, Tt, Aend);
Lnonswap:Ts At;

@@swapNeg Ax, Ay, Tx, Ty, Aend // [invariant: Tx = -Ax and Ty = -Ay] Ax <- Ay ; Ay <- Ax
Ax; Ay; Tx Ay; Ty Ax;
Z Z Aend;


@@neg Ad, A, T, Aend
A Z; Z T; Ad; T Ad; T;
Z Z Aend;

@@inv Ad, A, T, Aend
A Z; Z T; Ad; T Ad; T;
Dec Ad; Z Z Aend;

define(`DestructiveBitwise',`
PureSubleq(`@@$1') Rd, Rs, Rt, S0, S1, Aend
Rd; Dec Rd;
CW S0;
L1:Inc Rd;
L0:Inc S0 LBody;
LFinish:S0; S1 S1 Aend;
LBody:$(@@sl1m Rd, LBody2);
LBody2:$(@@sl1cj Rs, Ls0, Ls1);
Ls0:$(@@sl1cj Rt, L$2, L$3);
Ls1:$(@@sl1cj Rt, L$4, L$5);
ifelse(ARCH,`subleqr',`
@@$1 Rd, Rs, Rt, S0, S1, Aend
$(@@$1O Rd, Rs, Rt, S0, S1, Aend);
')
')

dnl destructiveBitwiseR(int a, int b, bool c[4], bool (*f)(int, int, void*, void*)) {
dnl 	int out = 0;
dnl 	int p = 1; // S0
dnl 	while (f(a, b, cont, fin), goto fin, cont:) {
dnl 		
dnl 		int a0 = a & 1;
dnl 		a >>= 1;
dnl 		int b0 = b & 1;
dnl 		b >>= 1;
dnl 		if (c[a * 2 + b]) {
dnl 			out += p;
dnl 		}
dnl 		p <<= 1;
dnl 	}
dnl 	fin:
dnl 	return out;
dnl }
dnl f: jump to LBody if the loop condition holds, otherwise next or jump to LFinish, able to use S1.

define(`DestructiveBitwiseR',`
ifelse(ARCH,`subleqr',`
@@$1 Rd, Rs, Rt, S0, S1, Aend
Rd; Inc S0;
L1:Inc Rd;
dnl L0:$6;
L0:$($6 Rs, Rt, LBody, LFinish);
LFinish:S0; S1 S1 Aend;
LBody:$(@@sl1m Rd, LBody2);
LBody2:$(@@sl1cj Rs, Ls0, Ls1);
Ls0:$(@@sl1cj Rt, L$2, L$3);
Ls1:$(@@sl1cj Rt, L$4, L$5);
dnl @@$1 Rd, Rs, Rt, S0, S1, Aend
dnl $(@@$1O Rs, Rt, , S0, S1, Aend);
')
')


DestructiveBitwise(`andDSub',`0',`0',`0',`1')
DestructiveBitwise(`orDSub',`0',`1',`1',`1')
DestructiveBitwise(`xorDSub',`0',`1',`1',`0')
DestructiveBitwise(`norDSub',`1',`0',`0',`0')

@@addRfinish A, B, Cont, Fin
$(@@jznz Rs, Fin, F1); F1:$(@@jznz Rt, Fin, Cont);

@@orRfinish A, B, Cont, Fin
$(@@jznz Rs, F1, Cont); F1:$(@@jznz Rt, Fin, Cont);

DestructiveBitwiseR(`andDRSub',`0',`0',`0',`1',`@@addRfinish')
DestructiveBitwiseR(`orDRSub',`0',`1',`1',`1',`@@orRfinish')
DestructiveBitwiseR(`xorDRSub',`0',`1',`1',`0',`@@orRfinish') dnl it is better to use xorRfinish
DestructiveBitwise(`norDRSub',`1',`0',`0',`0')

@@lwSub1 Rt, MAddr, Aend // Rt <- mem[-MAddr]
MAddr X;
X:0 Z; X; Rt; Z Rt; Z Z Aend;

@@swSub1 Rt, MAddr, Aend // mem[-MAddr] <- Rt
Rt Z; MAddr X1; MAddr X2; MAddr X3;
X1:0 X2:0; Z X3:0; X1; X2; X3; Z Z Aend;

dnl @@lwSub Rt, Base, Offset, Aend
dnl Base T0; Offset T0; 
dnl $(@@lwSub1 Rt, T0, LFinish);
dnl LFinish:T0 T0 Aend;

@@divuSub Hi, Lo, Rs, Rt, T0, T1, T2, T3, End
Hi; Rs T1;
CW T0;
Loop:Inc T0 LBody;
LFinish:T0; Rs; T1 Rs; T3; T1 T1 End;
LBody:$(@@sl1m Lo); $(@@sl1d Hi, Rs, LBody2);
LBody2:T2; Rs T2; Rt Rs; $(@@jnzp Rs, LResume, LNext, LNext);
LNext:End; // we must fix
LResume:Rs; T2 Rs; T2 T2;
$(@@addc Hi, Lo, Rt, T2, Loop);

ifelse(ARCH,`subleqr',`
@@luiSub Rt, Imm, T0, T1, End
Rt; T0; Imm T1; UpperOne T0;
Loop:$(@@jezo Imm, Le, LFinish, Lo);
Lo:T0 Rt; 
Le:$(@@sl1m T0, Lshr);
Lshr:$(@@srl1m Imm, Loop);
LFinish:T0; T1 Imm; T1 T1 End;
CH:16 UpperOne:65536;
')

PureSubleq(`@@luiSub') Rd, Imm, T0, T1, Aend
$(@@sllsub Rd, Imm, CH, T0, T1, Aend);
CH:16;

PureSubleq(`@@rl') Rd, Sa, S0, S1, Aend // left rotate Rd by Sa : Rd <- {Rd[w-sa-1:0], Rd[w-1:w-sa]}
Sa S0;
Loop:Inc S0 LBody;
LFinish:S0; S1 S1 Aend;
LBody:$(@@rl1m Rd, Loop);

PureSubleq(`@@rl8') Rd, Sa, S0, S1, Aend // left rotate Rd by Sa : Rd <- {Rd[w-8*sa-1:0], Rd[w-1:w-8*sa]}
Sa S0;
Loop:Inc S0 LBody;
LFinish:S0; S1 S1 Aend;
LBody:$(@@rl8m Rd, Loop);

PureSubleq(`@@rl16') Rd, Sa, S0, S1, Aend // left rotate Rd by Sa : Rd <- {Rd[w-16*sa-1:0], Rd[w-1:w-16*sa]}
Sa S0;
Loop:Inc S0 LBody;
LFinish:S0; S1 S1 Aend;
LBody:$(@@rl16m Rd, Loop);

ifelse(ARCH,`subleqr',`
@@rl Rd, Sa, S0, S1, Aend // left rotate Rd by Sa : Rd <- {Rd[w-sa-1:0], Rd[w-1:w-sa]}
$(@@rlO Rd, Sa, S0, S1, Aend);

@@rl16 Rd, Sa, S0, S1, Aend
$(@@rl16O Rd, Sa, S0, S1, Aend);

@@rl8 Rd, Sa, S0, S1, Aend
$(@@rl8O Rd, Sa, S0, S1, Aend);

@@rr Rd, Sa, S0, S1, Aend
Sa S0;
Loop:Inc S0 LBody;
LFinish:S0; S1 S1 Aend;
LBody:$(@@rr1m Rd, Loop);

@@rr8 Rd, Sa, S0, S1, Aend
Sa S0;
Loop:Inc S0 LBody;
LFinish:S0; S1 S1 Aend;
LBody:$(@@rr8m Rd, Loop);

@@rr16 Rd, Sa, S0, S1, Aend
Sa S0;
Loop:Inc S0 LBody;
LFinish:S0; S1 S1 Aend;
LBody:$(@@rr16m Rd, Loop);
')

PureSubleq(`@@rlsl') Rd, Rs, Sa, S0, S1, Aend // left rotate Rd by Sa, left shift Rs by Sa : Rd <- {Rd[w-sa-1:0], Rd[w-1:w-sa]} ; Rs <- {Rs[w-sa-1:0], sa%0}
Sa S0;
Loop:Inc S0 LBody;
LFinish:S0; S1 S1 Aend;
LBody:$(@@rl1m Rd, LBody2);
LBody2:$(@@sl1m Rs, Loop);
ifelse(ARCH,`subleqr',`
@@rlsl Rd, Rs, Sa, S0, S1, Aend // left rotate Rd by Sa, left shift Rs by Sa : Rd <- {Rd[w-sa-1:0], Rd[w-1:w-sa]} ; Rs <- {Rs[w-sa-1:0], sa%0}
$(@@rlslO Rd, Rs, Sa, S0, S1, Aend);

@@rrsl Rd, Rs, Sa, S0, S1, Aend // right rotate Rd by Sa, right shift Rs by Sa : Rd <- {Rd[sa-1:0], Rd[w-1:sa]} ; Rs <- {Rs[w-sa-1:0], sa%0}
Sa S0;
Loop:Inc S0 LBody;
LFinish:S0; S1 S1 Aend;
LBody:$(@@rr1m Rd, LBody2);
LBody2:$(@@srl1m Rs, Loop);
')

PureSubleq(`@@rlslm') Rd, Rs, Sa, S0, S1, Aend // left rotate Rd by Sa, left shift Rs by Sa : Rd <- {Rd[w-sa-1:0], Rs[w-1:w-sa]} ; Rs <- {Rs[w-sa-1:0], sa%0}
Sa S0;
Loop:Inc S0 LBody;
LFinish:S0; S1 S1 Aend;
LBody:$(@@rlsl1m Rd, Rs, Loop);

PureSubleq(`@@rlsli3m') Rd, Rs, Sa, S0, S1, Aend // left rotate Rd by Sa, left shift Rs by Sa : Rd <- {Rd[w-sa-1:0], Rs[w-1:w-sa]} ; Rs <- {Rs[w-sa-1:0], sa%0}
Sa S0;
Loop:Inc S0 LBody;
LFinish:S0; S1 S1 Aend;
LBody:$(@@rlsl8m Rd, Rs, Loop);

PureSubleq(`@@rlsli4m') Rd, Rs, Sa, S0, S1, Aend // left rotate Rd by Sa, left shift Rs by Sa : Rd <- {Rd[w-sa-1:0], Rs[w-1:w-sa]} ; Rs <- {Rs[w-sa-1:0], sa%0}
Sa S0;
Loop:Inc S0 LBody;
LFinish:S0; S1 S1 Aend;
LBody:$(@@rlsl16m Rd, Rs, Loop);

@@rlsl1m Rd, Rs, Aend // left rotate Rd by 1, left shift Rs by 1 : Rd <- {Rd[w-2:0], Rs[w-1]} ; Rs <- {Rs[w-2:0], sa%0}
LBody:$(@@sl1m Rd, LBody2);
LBody2:$(@@sl1ca Rd, Rs, Aend);

@@rlsl2m Rd, Rs, Aend
LBody:$(@@rlsl1m Rd, Rs, LBody2);
LBody2:$(@@rlsl1m Rd, Rs, Aend);

@@rlsl4m Rd, Rs, Aend
LBody:$(@@rlsl2m Rd, Rs, LBody2);
LBody2:$(@@rlsl2m Rd, Rs, Aend);

@@rlsl8m Rd, Rs, Aend
LBody:$(@@rlsl4m Rd, Rs, LBody2);
LBody2:$(@@rlsl4m Rd, Rs, Aend);

@@rlsl16m Rd, Rs, Aend
LBody:$(@@rlsl8m Rd, Rs, LBody2);
LBody2:$(@@rlsl8m Rd, Rs, Aend);

PureSubleq(`@@slrlm') Rd, Rs, Sa, S0, S1, Aend // left shift Rd by Sa, left rotate Rs by Sa : Rd <- {Rd[w-sa-1:0], Rs[w-1:w-sa]} ; Rs <- {Rs[w-sa-1:0], Rs[w-1:w-sa]}
Sa S0;
Loop:Inc S0 LBody;
LFinish:S0; S1 S1 Aend;
LBody:$(@@slrl1m Rd, Rs, S1, Loop);
ifelse(ARCH,`subleqr',`
@@slrlm Rd, Rs, Sa, S0, S1, Aend // left shift Rd by Sa, left rotate Rs by Sa : Rd <- {Rd[w-sa-1:0], Rs[w-1:w-sa]} ; Rs <- {Rs[w-sa-1:0], Rs[w-1:w-sa]}
$(@@slrlmO Rd, Rs, Sa, S0, S1, Aend);

@@rlslm Rd, Rs, Sa, S0, S1, Aend // left rotate Rd by Sa, left shift Rs by Sa : Rd <- {Rd[w-sa-1:0], Rs[w-1:w-sa]} ; Rs <- {Rs[w-sa-1:0], sa%0}
$(@@rlslmO Rd, Rs, Sa, S0, S1, Aend);

@@rrsrm Rd, Rs, Sa, S0, S1, Aend // right rotate Rd by Sa, right shift Rs by Sa : Rd <- {Rs[sa-1:0], Rd[w-1:sa]} ; Rs <- {sa%0, Rs[w-1:sa]}
Sa S0;
Loop:Inc S0 LBody;
LFinish:S0; S1 S1 Aend;
LBody:$(@@srl1m Rd, LBody2);
LBody2:$(@@srl1cr Rd, Rs, Loop);

@@rrsr1m Rd, Rs, S0, S1, Aend // right rotate Rd by 1, right shift Rs by 1 : Rd <- {Rs[1-1:0], Rd[w-1:1]} ; Rs <- {1%0, Rs[w-1:1]}
LBody:$(@@srl1m Rd, LBody2);
LBody2:$(@@srl1cr Rd, Rs, Aend);

@@rrsr2m Rd, Rs, S0, S1, Aend
$(@@rrsr1m Rd, Rs, S0, S1, L1);
L1:$(@@rrsr1m Rd, Rs, S0, S1, Aend);

@@rrsr4m Rd, Rs, S0, S1, Aend
$(@@rrsr2m Rd, Rs, S0, S1, L1);
L1:$(@@rrsr2m Rd, Rs, S0, S1, Aend);

@@rrsr8m Rd, Rs, S0, S1, Aend // right rotate Rd by 8, right shift Rs by 8 : Rd <- {Rs[8-1:0], Rd[w-1:8]} ; Rs <- {8%0, Rs[w-1:8]}
$(@@rrsr4m Rd, Rs, S0, S1, L1);
L1:$(@@rrsr4m Rd, Rs, S0, S1, Aend);

@@rrsr16m Rd, Rs, S0, S1, Aend // right rotate Rd by 16, right shift Rs by 16 : Rd <- {Rs[16-1:0], Rd[w-1:16]} ; Rs <- {16%0, Rs[w-1:16]}
$(@@rrsr8m Rd, Rs, S0, S1, L1);
L1:$(@@rrsr8m Rd, Rs, S0, S1, Aend);

@@srrrm Rd, Rs, Sa, S0, S1, Aend // right rotate Rd by Sa, right shift Rs by Sa : Rd <- {Rs[sa-1:0], Rd[w-1:sa]} ; Rs <- {Rs[sa-1:0], Rs[w-1:sa]}
Sa S0;
Loop:Inc S0 LBody;
LFinish:S0; S1 S1 Aend;
LBody:$(@@rrlsl1m Rd, Rs, S1, Loop);
dnl LBody:$(@@srl1m Rd, LBody2);
dnl LBody2:$(@@srl1cr Rd, Rs, Loop);
')

dnl  precondition: F + S <= w
dnl  sll(Rs, w - S)
dnl  Rd: {Rd[w-1:0]} Rs: {Rs[S-1:0], Rs[w-1:S]}
dnl  rl(Rd, w - F - S)
dnl  Rd: {Rd[F+S-1:0], Rd[w-1:F+S]} Rs: {Rs[S-1:0], Rs[w-1:S]}
dnl  rlslm(Rd, Rs, S)
dnl  Rd: {Rd[F-1:0], Rd[w-1:F+S], Rs[S-1:0]} Rs: {Rs[w-1:S], S%0}
dnl  rl(Rd, F)
dnl  Rd: {Rd[w-1:F+S], Rs[S-1:0], Rd[F-1:0]} Rs: {Rs[w-1:0], S%0}
PureSubleq(`@@svi') Rd, Rs, From, Size, S0, S1, Sf, Snf, Aend // Rd <- {Rd[w-1:From+1], Rs[Size-1:0], Rd[From-Size,0]} ; modify Rd, Rs
dnl $(@@jznz Size, Aend, LDo);
LDo:Size Snf; CW Z; Z Snf; Z;
$(@@sllsub Sf, Rs, Snf, S0, S1, L0);
L0:Sf Z; Sf; Rs; Z Rs; Z;
dnl
From Sf; From Snf;
L1:$(@@rl Rd, Snf, S0, S1, L2);
L2:$(@@rlslm Rd, Rs, Size, S0, S1, L3);
L3:$(@@rl Rd, From, S0, S1, LFinish);
LFinish:From; Sf From;
Sf; Snf Snf Aend;


dnl  precondition: F + S <= wb
dnl  sll8(Rs, wb - S)
dnl  Rd: {Rd[w-1:0]} Rs: {Rs[S-1:0], Rs[w-1:S]}
dnl  rl8(Rd, wb - F - S)
dnl  Rd: {Rd[F+S-1:0], Rd[w-1:F+S]} Rs: {Rs[S-1:0], Rs[w-1:S]}
dnl  rlsl8m(Rd, Rs, S)
dnl  Rd: {Rd[F-1:0], Rd[w-1:F+S], Rs[S-1:0]} Rs: {Rs[w-1:S], S%0}
dnl  rl8(Rd, F)
dnl  Rd: {Rd[w-1:F+S], Rs[S-1:0], Rd[F-1:0]} Rs: {Rs[w-1:0], S%0}
PureSubleq(`@@sbi') Rd, Rs, From, S0, S1, Sf, Snf, Aend // Rd <- {Rd[w-1:From+1], Rs[Size-1:0], Rd[From-Size,0]} ; modify Rd, Rs
dnl $(@@jznz Size, Aend, LDo);
LDo:Dec Snf; CWb Z; Z Snf; Z;
$(@@slli3sub Sf, Rs, Snf, S0, S1, L0);
L0:Sf Z; Sf; Rs; Z Rs; Z;
dnl
From Sf; From Snf;
L1:$(@@rl8 Rd, Snf, S0, S1, L2);
L2:$(@@rlsl8m Rd, Rs, L3);
L3:$(@@rl8 Rd, From, S0, S1, LFinish);
LFinish:From; Sf From;
Sf; Snf Snf Aend;
CWb:4;

PureSubleq(`@@shi') Rd, Rs, From, S0, S1, Sf, Snf, Aend // Rd <- {Rd[w-1:From+1], Rs[Size-1:0], Rd[From-Size,0]} ; modify Rd, Rs
dnl $(@@jznz Size, Aend, LDo);
LDo:Dec Snf; CWh Z; Z Snf; Z;
$(@@slli4sub Sf, Rs, Snf, S0, S1, L0);
L0:Sf Z; Sf; Rs; Z Rs; Z;
dnl
From Sf; From Snf;
L1:$(@@rl16 Rd, Snf, S0, S1, L2);
L2:$(@@rlsl16m Rd, Rs, L3);
L3:$(@@rl16 Rd, From, S0, S1, LFinish);
LFinish:From; Sf From;
Sf; Snf Snf Aend;
CWh:2;

ifelse(ARCH,`subleqr',`
@@svi Rd, Rs, From, Size, S0, S1, Sf, Snf, Aend // Rd <- {Rd[w-1:From+1], Rs[Size-1,0], Rd[From-Size,0]} ; modify Rd, Rs
From Z; From Z; Size Z; Z Sf; CW Sf; Z;
$(@@jnzp Sf, Lh, Ll, Ll);
Lh:Sf; $(@@svri Rd, Rs, From, Size, S0, S1, Sf, Snf, Aend);
Ll:Sf; $(@@svli Rd, Rs, From, Size, S0, S1, Sf, Snf, Aend);

@@sbi Rd, Rs, From, S0, S1, Sf, Snf, Aend // Rd <- {Rd[w-1:8*From+1], Rs[8-1,0], Rd[8*From-8,0]} ; modify Rd, Rs
CWbh Sf; From Sf Lh;
Ll:Sf; $(@@sbri Rd, Rs, From, S0, S1, Sf, Snf, Aend);
Lh:Sf; $(@@sbli Rd, Rs, From, S0, S1, Sf, Snf, Aend);
CWbh:(- 2);

@@shi Rd, Rs, From, S0, S1, Sf, Snf, Aend // Rd <- {Rd[w-1:8*From+1], Rs[8-1,0], Rd[8*From-8,0]} ; modify Rd, Rs
CWhh Sf; From Sf Lh;
Ll:Sf; $(@@shri Rd, Rs, From, S0, S1, Sf, Snf, Aend);
Lh:Sf; $(@@shli Rd, Rs, From, S0, S1, Sf, Snf, Aend);
CWhh:(- 1);

dnl  precondition: From + Size <= w, prefer From + S >= w / 2
dnl  F2 = w - From
dnl  S <= F2
dnl  rl(Rd, F2)
dnl  Rd: {Rd[w-F2-1:0], Rd[w-1:w-F2+S], Rd[w-F2+S-1:w-F2]} Rs: {Rs[w-1:S], Rs[S-1:0]}
dnl  rrsrm(Rd, Rs, S)
dnl  Rd: {Rs[S-1:0], Rd[F2-1:0], Rd[w-1:w-F2+S]} Rs: {S%0, Rs[w-1:S]}
dnl  rr(Rd, F2-S)
dnl  Rd: {Rd[w-1:w-F2+S], Rs[S-1:0], Rd[w-F2-1:0]} Rs: {S%0, Rs[w-1:S]}
@@svli Rd, Rs, From, Size, S0, S1, Sf, Snf, Aend // Rd <- {Rd[w-1:From+1], Rs[Size-1,0], Rd[From-Size,0]} ; modify Rd, Rs
From Sf; CW Z; Z Sf; Z;
$(@@rl Rd, Sf, S0, S1, L1);
L1:$(@@rrsrm Rd, Rs, Size, S0, S1, L2);
L2:Size Sf;
$(@@rr Rd, Sf, S0, S1, L3);
L3:Sf Sf Aend;

@@sbli Rd, Rs, From, S0, S1, Sf, Snf, Aend // Rd <- {Rd[w-1:8*From+1], Rs[7,0], Rd[8*From-8,0]} ; modify Rd, Rs
From Sf; CWb Z; Z Sf; Z;
$(@@rl8 Rd, Sf, S0, S1, L1);
L1:$(@@rrsr8m Rd, Rs, S0, S1, L2);
L2:Dec Sf;
$(@@rr8 Rd, Sf, S0, S1, L3);
L3:Sf Sf Aend;
CWb:4;

@@shli Rd, Rs, From, S0, S1, Sf, Snf, Aend // Rd <- {Rd[w-1:8*From+1], Rs[7,0], Rd[8*From-8,0]} ; modify Rd, Rs
From Sf; CWh Z; Z Sf; Z;
$(@@rl16 Rd, Sf, S0, S1, L1);
L1:$(@@rrsr16m Rd, Rs, S0, S1, L2);
L2:Dec Sf;
$(@@rr16 Rd, Sf, S0, S1, L3);
L3:Sf Sf Aend;
CWh:2;

dnl  precondition: F + S <= w, prefer F + S <= w / 2
dnl  F = From
dnl  rr(Rd, F)
dnl  Rd: {Rd[F-1:0], Rd[w-1:F+S], Rd[F+S-1:F]} Rs: {Rs[w-1:S], Rs[S-1:0]}
dnl  rrsrm(Rd, Rs, S)
dnl  Rd: {Rs[S-1:0], Rd[F-1:0], Rd[w-1:F+S]} Rs: {S%0, Rs[w-1:S]}
dnl  rl(Rd, S)
dnl  Rd: {Rd[w-1:F+S], Rs[S-1:0], Rd[F-1:0]} Rs: {S%0, Rs[w-1:S]}
@@svri Rd, Rs, From, Size, S0, S1, Sf, Snf, Aend // Rd <- {Rd[w-1:From+1], Rs[Size-1,0], Rd[From-Size,0]} ; modify Rd, Rs
$(@@rr Rd, From, S0, S1, L1);
L1:$(@@rrsrm Rd, Rs, Size, S0, S1, L2);
L2:Size Z; From Z; Z Sf; Z;
$(@@rl Rd, Sf, S0, S1, L3);
L3:Sf Sf Aend;

@@sbri Rd, Rs, From, S0, S1, Sf, Snf, Aend
$(@@rr8 Rd, From, S0, S1, L1);
L1:$(@@rrsr8m Rd, Rs, S0, S1, L2);
L2:Dec Z; From Z; Z Sf; Z;
$(@@rl8 Rd, Sf, S0, S1, L3);
L3:Sf Sf Aend;

@@shri Rd, Rs, From, S0, S1, Sf, Snf, Aend
$(@@rr16 Rd, From, S0, S1, L1);
L1:$(@@rrsr16m Rd, Rs, S0, S1, L2);
L2:Dec Z; From Z; Z Sf; Z;
$(@@rl16 Rd, Sf, S0, S1, L3);
L3:Sf Sf Aend;
')

ifelse(ARCH,`subleqr',`
@@addrw Rd, Rs, S0, S1, Aend // Rd <- {2%b0, Rs[w-1:2]}
Rs Z; Rd; Z Rd; Z;
$(@@srl2m Rd, Aend);
')

ifelse(ARCH,`subleqr',`
@@addrh Rd, Rt, Rs, S0, Aend // Rd <- {2%b0, Rs[w-1:2]}, Rt <- Rs[1]
Rt; Rs Z; Rd; Z Rd; Z;
$(@@srl1m Rd, L2);
L2:$(@@srl1ca Rt, Rd, Aend);
L3:$(@@srl1m Rd, Aend);
L4:S0 S0 Aend;
')

ifelse(ARCH,`subleqr',`
@@addrb Rd, Rt, Rs, S0, Aend // Rd <- {2%b0, Rs[w-1:2]}, Rt <- Rs[1:0]
Rt; Rs Z; Rd; Z Rd; Z;
$(@@srl1ca S0, Rd, L1);
L1:$(@@sl1m S0, L1b);
L1b:$(@@srl1ca S0, Rd, L2);
L2:$(@@srl1ca Rt, S0, L3);
L3:$(@@sl1m Rt, L3b);
L3b:$(@@srl1ca Rt, S0, L4);
L4:S0 S0 Aend;
')

PureSubleq(`@@addrw') Rd, Rs, S0, S1, Aend // Rd <- {1%b0, Rs[w-1:2]}
Rs Z; Rd; Z Rd; Z;
L1:$(@@rl30m Rd, L2);
L2:Rd Z; Z S0; Z;
$(@@sl1ca S1, S0, L3);
L3:$(@@sl1m S1, L4);
L4:$(@@sl1ca S1, S0, L5);
L5:S0; S1 S0; S1;
Ll0:Inc S0 Ll1;
S0 S0 Aend;
Ll1:De2p31 Rd; Inc S0 Ll2;
S0 S0 Aend;
Ll2:De2p31 Rd; Inc S0 Ll3;
S0 S0 Aend;
Ll3:De2p31 Rd;
S0 S0 Aend;
De2p31:(shift 1 30);

PureSubleq(`@@addrh') Rd, Rt, Rs, S0, Aend // Rd <- {1%b0, Rs[w-1:2]}, Rt <- Rs[1]
Rt; Rs Z; Rd; Z Rd; Z;
L1:$(@@rl30m Rd, L2);
L2:Rd Z; Z S0; Z;
$(@@sl1ca Rt, S0, L3);
L3:$(@@sl1m Rt, L4);
L4:$(@@sl1ca Rt, S0, L5);
L5:S0; Rt S0; Rt;
Ll0:Inc S0 Ll1;
S0 S0 Aend;
Ll1:De2p31 Rd; Inc S0 Ll2;
S0 S0 Aend;
Ll2:De2p31 Rd; Inc Rt; Inc S0 Ll3;
S0 S0 Aend;
Ll3:De2p31 Rd;
S0 S0 Aend;
De2p31:(shift 1 30);

PureSubleq(`@@addrb') Rd, Rt, Rs, S0, Aend // Rd <- -{1%b0, Rs[w-1:2]}, Rt <- Rs[1:0]
Rt; Rs Z; Rd; Z Rd; Z;
L1:$(@@rl30m Rd, L2);
L2:Rd Z; Z S0; Z;
$(@@sl1ca Rt, S0, L3);
L3:$(@@sl1m Rt, L4);
L4:$(@@sl1ca Rt, S0, L5);
L5:S0; Rt S0;
Ll0:Inc S0 Ll1;
S0 S0 Aend;
Ll1:De2p31 Rd; Inc S0 Ll2;
S0 S0 Aend;
Ll2:De2p31 Rd; Inc S0 Ll3;
S0 S0 Aend;
Ll3:De2p31 Rd;
S0 S0 Aend;
De2p31:(shift 1 30);

dnl  F + S <= w
dnl  Rs: {Rd[w-1:0]} Rs: {0}
dnl  rl(Rs, w - F - S)
dnl  Rs: {Rs[F+S-1:0], Rs[w-1:F+S]} Rd: {0}
dnl  slrlm(Rd, Rs, S)
dnl  Rs: {Rs[F-1:0], Rs[w-1:F+S], Rs[F+S-1:F]} Rd: {0,Rs[F+S-1:F]}
dnl  rl(Rs, F)
PureSubleq(`@@lvui') Rd, Rs, From, Size, S0, S1, Sf, Snf, Aend // Rd <- {Rd[From+Size-1:From]}
Rd;
dnl Inc From;
From Sf; CW From; From Snf; Size Snf;
L1:$(@@rl Rs, Snf, S0, S1, L2);
L2:$(@@slrlm Rd, Rs, Size, S0, S1, L3);
L3:From; Sf From;
$(@@rl Rs, From, S0, S1, LFinish);
LFinish:dnl
dnl Dec From;
Sf; Snf Snf Aend;

dnl  F <= 3
dnl  Rs: {Rd[w-1:0]} Rs: {0}
dnl  rl8(Rs, 4 - F - 1)
dnl  Rs: {Rs[8F+8-1:0], Rs[w-1:8F+8]} Rd: {0}
dnl  slrlm8(Rd, Rs, 1)
dnl  Rs: {Rs[8F-1:0], Rs[w-1:8F+8], Rs[8F+8-1:8F]} Rd: {0,Rs[8F+8-1:8F]}
dnl  rl8(Rs, F)

PureSubleq(`@@lbui') Rd, Rs, From, S0, S1, Sf, Snf, Aend // Rd <- {Rd[From+Size-1:From]}
dnl $(PureSubleq(`@@lbuiBig') Rd, Rs, From, S0, S1, Sf, Snf, Aend);
$(PureSubleq(`@@lbuiSmall') Rd, Rs, From, S0, S1, Sf, Snf, Aend);

PureSubleq(`@@lhui') Rd, Rs, From, S0, S1, Sf, Snf, Aend // Rd <- {Rd[From+Size-1:From]}
dnl $(PureSubleq(`@@lhuiBig') Rd, Rs, From, S0, S1, Sf, Snf, Aend);
$(PureSubleq(`@@lhuiSmall') Rd, Rs, From, S0, S1, Sf, Snf, Aend);

PureSubleq(`@@lbuiBig') Rd, Rs, From, S0, S1, Sf, Snf, Aend // Rd <- {Rd[From+Size-1:From]}
Rd;
dnl Inc From;
From Sf; CWb From; From Snf; Dec Snf;
L1:$(@@rl8 Rs, Snf, S0, S1, L2);
L2:$(@@slrl8m Rd, Rs, S0, L3);
L3:From; Sf From;
$(@@rl8 Rs, From, S0, S1, LFinish);
LFinish:dnl
dnl Dec From;
Sf; Snf Snf Aend;
CWb:4;

PureSubleq(`@@lhuiBig') Rd, Rs, From, S0, S1, Sf, Snf, Aend // Rd <- {Rd[From+Size-1:From]}
Rd;
dnl Inc From;
From Sf; CWh From; From Snf; Dec Snf;
L1:$(@@rl16 Rs, Snf, S0, S1, L2);
L2:$(@@slrl16m Rd, Rs, S0, L3);
L3:From; Sf From;
$(@@rl16 Rs, From, S0, S1, LFinish);
LFinish:dnl
dnl Dec From;
Sf; Snf Snf Aend;
CWh:2;

PureSubleq(`@@lbuiSmall')  Rd, Rs, From, S0, S1, Sf, Snf, Aend // Rd <- {0, Rs[w-1:w-8]}
From Temp;
$(@@rl1m From, L1);
L1:$(@@rl2m From, L2);
L2:$(@@lvui Rd, Rs, From, Size, S0, S1, Sf, Snf, L3);
L3:From; Temp From; Temp Temp Aend;
Size:8 Temp:0;

PureSubleq(`@@lhuiSmall')  Rd, Rs, From, S0, S1, Sf, Snf, Aend // Rd <- {0, Rs[w-1:w-8]}
From Temp;
L1:$(@@rl4m From, L2);
L2:$(@@lvui Rd, Rs, From, Size, S0, S1, Sf, Snf, L3);
L3:From; Temp From; Temp Temp Aend;
Size:16 Temp:0;

ifelse(ARCH,`subleqr',`
@@lvui Rd, Rs, From, Size, S0, S1, Sf, Snf, Aend // Rd <- {(w-S)%0, Rd[From+Size-1,From]} ; modify Rs
From Z; From Z; Size Z; Z Sf; CW Sf; Z;
$(@@jnzp Sf, Lh, Ll, Ll);
Lh:Sf; $(@@lvuri Rd, Rs, From, Size, S0, S1, Sf, Snf, Aend);
Ll:Sf; $(@@lvuli Rd, Rs, From, Size, S0, S1, Sf, Snf, Aend);

@@lbui Rd, Rs, From, S0, S1, Sf, Snf, Aend // Rd <- {(w-S)%0, Rd[From+Size-1,From]} ; modify Rs
CWbh Sf; From Sf Ll;
Lh:Sf; $(@@lburi Rd, Rs, From, S0, S1, Sf, Snf, Aend);
Ll:Sf; $(@@lbuli Rd, Rs, From, S0, S1, Sf, Snf, Aend);
CWbh:(- 2);

@@lhui Rd, Rs, From, S0, S1, Sf, Snf, Aend // Rd <- {(w-S)%0, Rd[From+Size-1,From]} ; modify Rs
CWbh Sf; From Sf Ll;
Lh:Sf; $(@@lhuri Rd, Rs, From, S0, S1, Sf, Snf, Aend);
Ll:Sf; $(@@lhuli Rd, Rs, From, S0, S1, Sf, Snf, Aend);
CWbh:(- 1);

@@lhuri Rd, Rs, From, S0, S1, Sf, Snf, Aend
$(@@lhuriSmall Rd, Rs, From, S0, S1, Sf, Snf, Aend);

@@lhuli Rd, Rs, From, S0, S1, Sf, Snf, Aend
$(@@lhuliSmall Rd, Rs, From, S0, S1, Sf, Snf, Aend);

@@lburi Rd, Rs, From, S0, S1, Sf, Snf, Aend
$(@@lburiBig Rd, Rs, From, S0, S1, Sf, Snf, Aend);

@@lbuli Rd, Rs, From, S0, S1, Sf, Snf, Aend
$(@@lbuliBig Rd, Rs, From, S0, S1, Sf, Snf, Aend);

@@lhuriSmall Rd, Rs, From, S0, S1, Sf, Snf, Aend // Rd <- {0, Rs[w-1:w-8]}
From Temp;
L1:$(@@rl4m From, L2);
L2:$(@@lvuri Rd, Rs, From, Size, S0, S1, Sf, Snf, L3);
L3:From; Temp From; Temp Temp Aend;
Size:16 Temp:0;

@@lhuliSmall Rd, Rs, From, S0, S1, Sf, Snf, Aend // Rd <- {0, Rs[w-1:w-8]}
From Temp;
L1:$(@@rl4m From, L2);
L2:$(@@lvuli Rd, Rs, From, Size, S0, S1, Sf, Snf, L3);
L3:From; Temp From; Temp Temp Aend;
Size:16 Temp:0;

@@lburiSmall Rd, Rs, From, S0, S1, Sf, Snf, Aend // Rd <- {0, Rs[w-1:w-8]}
From Temp;
$(@@rl1m From, L1);
L1:$(@@rl2m From, L2);
L2:$(@@lvuri Rd, Rs, From, Size, S0, S1, Sf, Snf, L3);
L3:From; Temp From; Temp Temp Aend;
Size:8 Temp:0;

@@lbuliSmall Rd, Rs, From, S0, S1, Sf, Snf, Aend // Rd <- {0, Rs[w-1:w-8]}
From Temp;
$(@@rl1m From, L1);
L1:$(@@rl2m From, L2);
L2:$(@@lvuli Rd, Rs, From, Size, S0, S1, Sf, Snf, L3);
L3:From; Temp From; Temp Temp Aend;
Size:8 Temp:0;

dnl  F + S <= w
dnl  F2 = w - F
dnl  Rs: {Rd[w-1:0]} Rs: {0}
dnl  rl(Rs, w - F - S)
dnl  Rs: {Rs[F-1:0], Rs[w-1:F]} Rd: {0}
dnl  slrlm(Rd, Rs, S)
dnl  Rs: {Rs[F-S-1:0], Rs[w-1:F], Rs[F-1:F-S]} Rd: {0,Rs[F-1:F-S]}
dnl  rr(Rs, w - F)
@@lvuli Rd, Rs, From, Size, S0, S1, Sf, Snf, Aend // Rd <- {0, Rs[w-1:w-Size]}
Rd;
dnl Inc From;
From Sf; CW From; From Snf; Size Snf;
L1:$(@@rl Rs, Snf, S0, S1, L2);
L2:$(@@slrlm Rd, Rs, Size, S0, S1, L3);
L3:Size Z; Z Snf; Z;
$(@@rr Rs, Snf, S0, S1, LFinish);
LFinish:dnl
dnl Dec From;
From; Sf From; Sf; Snf Snf Aend;

@@lbuliBig Rd, Rs, From, S0, S1, Sf, Snf, Aend // Rd <- {0, Rs[w-1:w-8]}
Rd;
dnl Inc From;
From Sf; CWb From; From Snf; Dec Snf;
L1:$(@@rl8 Rs, Snf, S0, S1, L2);
L2:$(@@slrl8m Rd, Rs, S0, L3);
L3:Dec Z; Z Snf; Z;
$(@@rr8 Rs, Snf, S0, S1, LFinish);
LFinish:dnl
dnl Dec From;
From; Sf From; Sf; Snf Snf Aend;
CWb:4;

@@lhuliBig Rd, Rs, From, S0, S1, Sf, Snf, Aend // Rd <- {0, Rs[w-1:w-16]}
Rd;
dnl Inc From;
From Sf; CWh From; From Snf; Dec Snf;
L1:$(@@rl16 Rs, Snf, S0, S1, L2);
L2:$(@@slrl16m Rd, Rs, S0, L3);
L3:Dec Z; Z Snf; Z;
$(@@rr16 Rs, Snf, S0, S1, LFinish);
LFinish:dnl
dnl Dec From;
From; Sf From; Sf; Snf Snf Aend;
CWh:2;

dnl  F + S <= w
dnl  Rs: {Rd[w-1:0]} Rs: {0}
dnl  rr(Rs, F + S)
dnl  Rs: {Rs[F+S-1:0], Rs[w-1:F+S]} Rd: {0}
dnl  slrlm(Rd, Rs, S)
dnl  Rs: {Rs[F-1:0], Rs[w-1:F+S], Rs[F+S-1:F]} Rd: {0,Rs[F+S-1:F]}
dnl  rl(Rs, F)
@@lvuri Rd, Rs, From, Size, S0, S1, Sf, Snf, Aend // Rd <- {0, Rs[w-1:w-Size]}
Rd;
dnl Inc From;
From Sf; Size Z; Z From; Z;
L1:$(@@rr Rs, From, S0, S1, L2);
L2:$(@@slrlm Rd, Rs, Size, S0, S1, L3);
L3:From; Sf From;
$(@@rl Rs, From, S0, S1, LFinish);
LFinish:dnl
Sf Sf Aend;

@@lburiBig Rd, Rs, From, S0, S1, Sf, Snf, Aend // Rd <- {0, Rs[w-1:w-8]}
Rd;
dnl Inc From;
From Sf; Dec Z; Z From; Z;
L1:$(@@rr8 Rs, From, S0, S1, L2);
L2:$(@@slrl8m Rd, Rs, S0, L3);
L3:From; Sf From;
$(@@rl8 Rs, From, S0, S1, LFinish);
LFinish:dnl
Sf Sf Aend;

@@lhuriBig Rd, Rs, From, S0, S1, Sf, Snf, Aend // Rd <- {0, Rs[w-1:w-16]}
Rd;
dnl Inc From;
From Sf; Dec Z; Z From; Z;
L1:$(@@rr16 Rs, From, S0, S1, L2);
L2:$(@@slrl16m Rd, Rs, S0, L3);
L3:From; Sf From;
$(@@rl16 Rs, From, S0, S1, LFinish);
LFinish:dnl
Sf Sf Aend;
')

@@lbuSub Rd, Offset, Base, SAddr, SPos, St, Sd, S0, S1, S2, S3, Aend
Offset Z; Base Z;
Z S0; Z;
$(@@addrb S2, SPos, S0, S1, L0);
L0:S0; S2 SAddr; S2;
$(@@lwSub1 St, SAddr, L1);
L1:$(@@lbui Rd, St, SPos, S0, S1, S2, S3, L3);
L3:St; Sd; SAddr; SPos SPos Aend;

@@sbSub Rs, Offset, Base, SAddr, SPos, St, Sd, S0, S1, S2, S3, Aend
Offset Z; Base Z;
Z S0; Z;
Rs Z; Z Sd; Z;
$(@@addrb S2, SPos, S0, S1, L0);
L0:S0; S2 SAddr; S2;
$(@@lwSub1 St, SAddr, L1);
L1:$(@@sbi St, Sd, SPos, S0, S1, S2, S3, L2);
L2:$(@@swSub1 St, SAddr, L3);
L3:St; Sd; SAddr; SPos SPos Aend;

@@lhuSub Rd, Offset, Base, SAddr, SPos, St, Sd, S0, S1, S2, S3, Aend
Offset Z; Base Z;
Z S0; Z;
$(@@addrh S2, SPos, S0, S1, L0);
L0:S0; S2 SAddr; S2;
$(@@lwSub1 St, SAddr, L1);
L1:$(@@lhui Rd, St, SPos, S0, S1, S2, S3, L3);
L3:St; Sd; SAddr; SPos SPos Aend;

@@shSub Rs, Offset, Base, SAddr, SPos, St, Sd, S0, S1, S2, S3, Aend
Offset Z; Base Z;
Z S0; Z;
Rs Z; Z Sd; Z;
$(@@addrh S2, SPos, S0, S1, L0);
L0:S0; S2 SAddr; S2;
$(@@lwSub1 St, SAddr, L1);
L1:$(@@shi St, Sd, SPos, S0, S1, S2, S3, L2);
L2:$(@@swSub1 St, SAddr, L3);
L3:St; Sd; SAddr; SPos SPos Aend;

@@swSub Rt, Base, Offset, SAddr, S0, S1, S2, S3, Aend
Offset Z; Base Z;
Z S0; Z;
$(@@addrw S3, S0, S1, S2, L0);
L0:S0; S3 SAddr; S3;
$(@@swSub1 Rt, SAddr, LFinish);
LFinish:S0; SAddr SAddr Aend;

@@lwSub Rt, Base, Offset, SAddr, S0, S1, S2, S3, Aend
Offset Z; Base Z;
Z S0; Z;
$(@@addrw S3, S0, S1, S2, L0);
L0:S0; S3 SAddr; S3;
$(@@lwSub1 Rt, SAddr, LFinish);
LFinish:S0; SAddr SAddr Aend;

PureSubleq(`@@lsbPrepare') Addr, From, Aend // From <- Addr[1:0], Addr <- Addr[w:2]
Rd;

ifelse(ARCH,`subleqr',`
@@lsbPrepare Addr, From, Aend // From <- Addr[1:0], Addr <- Addr[w:2]
From;
L1:$(@@srl1ca From, Addr, L2);
L2:$(@@sl1m From, L3);
L3:$(@@srl1ca From, Addr, Aend);

@@lshPrepare Addr, From, Aend // From <- Addr[1], Addr <- Addr[w:2]
From;
L1:$(@@srl1m Addr, L2);
L2:$(@@srl1ca From, Addr, Aend);
')
