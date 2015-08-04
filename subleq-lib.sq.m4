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

PureSubleq(`@@sllsub') Rd, Rt, Sa, S1, S2, Aend
Rt S2; Rd; S2 Rd;
Sa S1; // S1 = -Sa
Loop:Inc S1 LBody;
LFinish:S2; S1 S1 Aend;
LBody:S2 Rd; S2; Rd S2; Z Z Loop;

PureSubleq(`@@srlsub') Rd, Rt, Sa, S0, S1, Aend
Rt S1; Rd;
Sa S0; CW Sa;
Loop:Inc Sa LBody;
LFinish:Sa; S0 Sa; S0; Rt; S1 Rt; S1 S1 Aend;
LBody:$(@@sl1d Rd, Rt, Loop);

PureSubleq(`@@srasub') Rd, Rt, Sa, S0, S1, Aend
Rt S1; Rd;
$(@@jnzp Rt, Ln, Lzp, Lzp);
Ln:Dec Rd;
Lzp:Sa S0; CW Sa;
Loop:Inc Sa LBody;
LFinish:Sa; S0 Sa; S0; Rt; S1 Rt; S1 S1 Aend;
LBody:$(@@sl1d Rd, Rt, Loop);

ifelse(ARCH,`subleqr',`
@@sllsub Rd, Rt, Sa, S1, S2, Aend
Rd; Sa Rd; Threshold Rd LBig;
$(@@sllsubO Rd, Rt, Sa, S1, S2, Aend);
LBig:$(@@sllsubBig Rd, Rt, Sa, S1, S2, Aend);
Threshold:(- 10);

@@srlsub Rd, Rt, Sa, S1, S2, Aend
Rd; Sa Rd; Threshold Rd LBig;
$(@@srlsub1 Rd, Rt, Sa, S1, S2, Aend);
LBig:$(@@srlsubO Rd, Rt, Sa, S1, S2, Aend);
Threshold:(- 21);

@@srasub Rd, Rt, Sa, S1, S2, S3, Aend
Rd; Sa Rd; Threshold Rd LBig;
$(@@srasub1 Rd, Rt, Sa, S1, S2, S3, Aend);
LBig:$(@@srasubO Rd, Rt, Sa, S1, S2, Aend);
Threshold:(- 20);

@@sllsubBig Rd, Rt, Sa, S0, S1, Aend
Rt S1; Rd;
Sa S0; CW Sa;
Loop:Inc Sa LBody;
LFinish:Sa; S0 Sa; S0; Rt; S1 Rt; S1 S1 Aend;
LBody:$(@@srl1d Rt, Rd, Loop);

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

ifelse(ARCH,`subleqr',`
@@srl1d Ah, Al, Aend // {Ah, Al, 1%d_} <- {1%b0, Ah, Al}
$(@@srl1m Al, L1); L1:Z Ah (- Lzo);
Le:$(@@srl1m Ah, Aend);
Lzo:Inc Ah (- Lo); RDec Ah (- Le);
Lo:RDec Ah (- Lo2); Lo2:Inc Al (- Lo3); Lo3:$(@@srl1m Ah, Aend);
')

@@sl1dc Ad, Ah, Al, Aend // {Ad, Al} <- {(w-1)%b0, Al, 1%b0}; {1%_, Ah} <- {Ah, Ad[0]}
$(@@sl1m Ah, L1); L1:Ad;
Z Al Lzn;
Lp:$(@@sl1m Al, Aend);
Lzn:Inc Al Ln; Dec Al Lp;
Ln:Dec Al; Inc Ad; Inc Ah; $(@@sl1m Al, Aend);

@@sl1c Ad, Al, Aend // {Ad, Al} <- {(w-1)%b0, Al, 1%d0}
L1:Ad;
$(@@sl1ca Ad, Al, Aend);

@@sl1ca Ad, Al, Aend // {A, Al} <- {(w-1)%b0, Al, 1%d0} ; Ad <- Ad + A
Z Al Lzn;
Lp:$(@@sl1m Al, Aend);
Lzn:Inc Al Ln; Dec Al Lp;
Ln:Dec Al; Inc Ad; $(@@sl1m Al, Aend);

@@sl1cj Al, Az, Ao // {Ad, Al} <- {(w-1)%b0, Al, 1%d0} ; if Ad == 1 then jump Ao else jump Az
Z Al Lzn;
Lp:$(@@sl1m Al, Az);
Lzn:Inc Al Ln; Dec Al Lp;
Ln:Dec Al; $(@@sl1m Al, Ao);

ifelse(ARCH,`subleqr',`
@@srl1 Ad, As, Aend // {Ad, 1%_} <- {1%b0, As}
As Z (- L2); L2:Ad; Z Ad (- L4); L4:Z Ad (- L5); L5:Z Z Aend;

@@srl1m A, Aend // {A, 1%d_} <- {1%d0, A}
A Z (- L2); L2:Z A (- L3); L3:Z Z Aend;
')

@@sl1 Ad, As, Aend // {1%d_, Ad} <- {As, 1%d0}
As Z; Ad; Z Ad; Z Ad; Z Z Aend;

@@sl1m A, Aend // {1%d_, A} <- {A, 1%d0}
A Z; Z A; Z Z Aend;

@@jnzp A, An, Az, Ap // goto An if A < 0, Az if A = 0, Ap if A > 0
Z A Lzn; Z Z Ap;
Lzn:Inc A Ln; Dec A Az;
Ln:Dec A An;

@@jznz A, Az, Anz // goto Az if A = 0, Anz if A != 0
Z A Lzn; Z Z Ap;
Lzn:Inc A Ln; Dec A Az;
Ln:Dec A An;

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
Finish:T4 T4 End;

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


DestructiveBitwise(`andDSub',`0',`0',`0',`1')
DestructiveBitwise(`orDSub',`0',`1',`1',`1')
DestructiveBitwise(`xorDSub',`0',`1',`1',`0')
DestructiveBitwise(`norDSub',`1',`0',`0',`0')

@@lwSub1 Rt, MAddr, Aend // Rt <- mem[-MAddr] ; MAddr <- 0
MAddr X; MAddr;
X:0 MAddr; X; Rt; MAddr Rt; MAddr MAddr Aend;

@@swSub1 Rt, MAddr, Aend // mem[-MAddr] <- Rt ; MAddr <- 0
Z X1; Z X2; Z X3; Z; Rt Z;
X1:0 X2:0; Z X3:0; X1; X2; X3; Z Z Aend;

@@lwSub Rt, Base, Offset, Aend
Base T0; Offset T0; 
$(@@lwSub1 Rt, T0, Aend);

@@swSub Rt, Base, Offset, Aend
Base T0; Offset T0; 
$(@@swSub1 Rt, T0, Aend);

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
