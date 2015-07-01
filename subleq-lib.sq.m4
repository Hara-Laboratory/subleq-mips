changequote(<,>)
// Here quote mark in not possible to use.
// I use 4%b1101 instead of 4'1101.
changequote`'

define(`PureSubleq',ifelse(ARCH,`subleq',`$1',`$1O'))

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

@@sllsub Rd, Rt, Sa, S1, S2, Aend
Rt S2; Rd; S2 Rd;
Sa S1; // S1 = -Sa
Loop:Inc S1 LBody;
LFinish:S2; S1 S1 Aend;
LBody:S2 Rd; S2; Rd S2; Z Z Loop;

PureSubleq(`@@srlSub') Rd, Rt, Sa, S0, S1, Aend
Rt S1; Rd;
Sa S0; CW Sa;
Loop:Inc Sa LBody;
LFinish:Sa; S0 Sa; S0; Rt; S1 Rt; S1 S1 Aend;
LBody:$(@@sl1d Rd, Rt, Loop);

PureSubleq(`@@sraSub') Rd, Rt, Sa, S0, S1, Aend
Rt S1; Rd;
$(@@jnzp Rt, Ln, Lzp, Lzp);
Ln:Dec Rd;
Lzp:Sa S0; CW Sa;
Loop:Inc Sa LBody;
LFinish:Sa; S0 Sa; S0; Rt; S1 Rt; S1 S1 Aend;
LBody:$(@@sl1d Rd, Rt, Loop);

ifelse(ARCH,`subleqr',`
@@srlsub Rd, Rt, Sa, S1, S2, Aend
Rd; Sa Rd; Threshold Rd LSmall;
$(@@srlSubO Rd, Rt, Sa, S1, S2, Aend);
LSmall:$(@@srlsub1 Rd, Rt, Sa, S1, S2, Aend);
Threshold:(- 17);

@@srasub Rd, Rt, Sa, S1, S2, S3, Aend
Rd; Sa Rd; Threshold Rd LSmall;
$(@@sraSubO Rd, Rt, Sa, S1, S2, Aend);
LSmall:$(@@srasub1 Rd, Rt, Sa, S1, S2, S3, Aend);
Threshold:(- 17);

@@srlsub1 Rd, Rt, Sa, S1, S2, Aend
Rt S2 (- L2); L2:Rd; L3:S2 Rd (- L4);
L4:Sa S1; // S1 = -Sa
Loop:Inc S1 LBody;
LFinish:S2; S1 S1 Aend;
LBody:S2 Rd (- LB2); LB2:S2; Rd S2 (- LB4); LB4:Z Z Loop;

@@srasub1 Rd, Rt, Sa, S1, S2, S3, Aend
Rt S2 (- L2); L2:Rd; L3:S2 Rd (- L4);
L4:Sa S1; // S1 = -Sa
$(@@jnzp Rt, Ln, Loop, Loop);
Ln:Min S3;
Loop:Inc S1 LBody;
LFinish:S2; S3; S1 S1 Aend;
LBody:S2 Rd (- LB2); LB2:S2; S3 Rd; Rd S2 (- LB4); LB4:Z Z Loop;
')

@@sl1d Ah, Al, Aend // {1%d_, Ah, Al} <- {Ah, Al, 1%d0}
$(@@sl1m Ah, L1); L1:Z Al Lzn;
Lp:$(@@sl1m Al, Aend);
Lzn:Inc Al Ln; Dec Al Lp;
Ln:Dec Al; Inc Ah; $(@@sl1m Al, Aend);

@@sl1dc Ad, Ah, Al, Aend // {Ad, Al} <- {(w-1)%b0, Al, 1%b0}; {1%_, Ah} <- {Ah, Ad[0]}
$(@@sl1m Ah, L1); L1:Ad;
Z Al Lzn;
Lp:$(@@sl1m Al, Aend);
Lzn:Inc Al Ln; Dec Al Lp;
Ln:Dec Al; Inc Ad; Inc Ah; $(@@sl1m Al, Aend);

@@sl1c Ad, Al, Aend // {Ad, Al} <- {(w-1)%b0, Al, 1%d0}
L1:Ad;
Z Al Lzn;
Lp:$(@@sl1m Al, Aend);
Lzn:Inc Al Ln; Dec Al Lp;
Ln:Dec Al; Inc Ad; $(@@sl1m Al, Aend);

ifelse(ARCH,`subleqr',`
@@srl1 Ad, As, Aend // {Ad, 1%_} <- {1%b0, As}
As Z (- L2); L2:Ad; Z Ad (- L4); L4:Z Ad (- L5); L5:Z Z Aend;

@@srl1m A, Aend // {A, 1%d_} <- {1%d0, A}
A Z (- L2); L2:Z A (- L3); L3:Z Z Aend;
')

@@sl1 Ad, As, Aend
As Z; Ad; Z Ad; Z Ad; Z Z Aend;

@@sl1m A, Aend // {1%d_, A} <- {A, 1%d0}
A Z; Z A; Z Z Aend;

@@jnzp A, An, Az, Ap // goto An if A < 0, Az if A = 0, Ap if A > 0
Z A Lzn; Z Z Ap;
Lzn:Inc A Ln; Dec A Az;
Ln:Dec A An;

ifelse(ARCH,`subleqr',`
@@jezo A, Ae, Az, Ao // goto Az if zero, Ae if A is even, Ao if A is odd.
Z A (- Lzo); Z Z Ae;
Lzo:Inc A (- Lo); Lz:Min A (- Az);
Lo:Min A (- Ao);
')

@@jeo A, Ae, Ao // goto Ae if A is even, Ao if A is odd.
$(@@jezo A, Ae, Ae, Ao);

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
Loop:$(@@jezo Rs, LBodyE, LFinish, LBodyO);
LFinish:Ts Rs; Ts; Rt; Tt Rt; TRth; Tt Tt End;
LBodyO:$(@@addc Hi, Lo, Rt, T2, LBodyO1);
LBodyO1:TRth Z; Z Hi; Z;
LBodyE:$(@@sl1d TRth, Rt, LBodyE1);
LBodyE1:$(@@srl1m Rs, Loop);
')

@@neg Ad, A, T, Aend
A Z; Z T; Ad; T Ad; T; Z Z Aend;

@@inv Ad, A, T, Aend
$(@@neg Ad, A, T, L);
L:Dec Ad Aend;

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
