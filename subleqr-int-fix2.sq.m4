@srl1dTest Rh, Rl
$(@@sl1d Rh, Rl, End);

@mflo Rd,Lo
Rd;
Lo Z;
Z Rd;
Z Z End;

@mtlo Lo,Rs
Lo;
Rs Z;
Z Lo;
Z Z End;

@sltu Rd,Rs,Rt
Min Rs; Min Rt;
$(@@sltSub Rd,Rs,Rt,T0,Finish);
Finish:Min Rs; Min Rt; Z Z End;

@multD Hi, Lo, Rs, Rt // destructive
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

@mult Hi, Lo, Rs, Rt
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
Finish:Rs; T5 Rs; T5; Rt; T6 Rt; T6; T4 T4 End;

@slt Rd,Rs,Rt
$(@@sltSub Rd,Rs,Rt,T0,End);

@syscall Rd
Rd;
2 Z;
Z Rd;
Z Z End;

@addu Rd,Rs,Rt
$(@@add Rd, Rs, Rt, End);

@subu Rd,Rs,Rt
Rs Z;
Rt T0;
T0 Z;
T0;
Rd;
Z Rd;
Z Z End;

@shl1 Rd,Rs
Rs Z; // Z <- -Rs, it assumes Z = 0
Rs Z; // Z <- Z - Rt = -(Rs + Rt)
Rd; // clear Rd
Z Rd;
Z Z End;

@bne Rs, Rt, Offset, PCq // I assume Offset is already sign-extended.
	Rs T1; T1 T2; Rt T2;
	Inc T2 LFinish;
	Dec T2 LTaken;
	LFinish:T1; T2 T2 End;
	LTaken:Offset Z; Z PCq; Z Z LFinish;

@bnea Rs, Offset, PCq // I assume Offset is already sign-extended.
	Rs T1; T1 T2; Rs T2;
	Inc T2 LFinish;
	Dec T2 LTaken;
	LFinish:T1; T2 T2 End;
	LTaken:Offset Z; Z PCq; Z Z LFinish;

@sll Rd, Rt, Sa
$(@@sllsub Rd, Rt, Sa, T0, T1, End);

@srl Rd, Rt, Sa
$(@@srlsub Rd, Rt, Sa, T0, T1, End);

@sra Rd, Rt, Sa
$(@@srasub Rd, Rt, Sa, T0, T1, T2, End);

@srlS Rd, Rt, Sa // Original Subleq
Rt T1; Rd;
Sa T0; CW Sa;
Loop:Inc Sa LBody;
LFinish:Sa; T0 Sa; T0; Rt; T1 Rt; T1 T1 End;
LBody:$(@@sl1d Rd, Rt, Loop);

@sraO Rd, Rt, Sa
Rt T1; Rd;
$(@@jnzp Rt, Ln, Lzp, Lzp);
Ln:Dec Rd;
Lzp:Sa T0; CW Sa;
Loop:Inc Sa LBody;
LFinish:Sa; T0 Sa; T0; Rt; T1 Rt; T1 T1 End;
LBody:$(@@sl1d Rd, Rt, Loop);

@srl1dcTest Rd, Rh, Rl
$(@@sl1dc Rd, Rh, Rl, End);

@srl1Test Rd, Rs
$(@@srl1 Rd, Rs, End);

@multu Hi, Lo, Rs, Rt
$(@@multuSub Hi, Lo, Rs, Rt, T0, T1, T2, T3, End);

@jezoTest R
$(@@jezo R, Le, Lz, Lo);
Le:R; Inc R End; Z Z End;
Lz:R; Z Z End;
Lo:R; Dec R End; Z Z End;

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

@@srlsub Rd, Rt, Sa, S1, S2, Aend
Rt S2 (- L2); L2:Rd; L3:S2 Rd (- L4);
L4:Sa S1; // S1 = -Sa
Loop:Inc S1 LBody;
LFinish:S2; S1 S1 Aend;
LBody:S2 Rd (- LB2); LB2:S2; Rd S2 (- LB4); LB4:Z Z Loop;

@@srasub Rd, Rt, Sa, S1, S2, S3, Aend
Rt S2 (- L2); L2:Rd; L3:S2 Rd (- L4);
L4:Sa S1; // S1 = -Sa
$(@@jnzp Rt, Ln, Loop, Loop);
Ln:Min S3;
Loop:Inc S1 LBody;
LFinish:S2; S3; S1 S1 Aend;
LBody:S2 Rd (- LB2); LB2:S2; S3 Rd; Rd S2 (- LB4); LB4:Z Z Loop;

@@sl1d Ah, Al, Aend // {1'd_, Ah, Al} <- {Ah, Al, 1'd0}
$(@@sl1m Ah, L1); L1:Z Al Lzn;
Lp:$(@@sl1m Al, Aend);
Lzn:Inc Al Ln; Dec Al Lp;
Ln:Dec Al; Inc Ah; $(@@sl1m Al, Aend);

@@sl1dc Ad, Ah, Al, Aend // {Ad, Al} <- {(w-1)'b0, Al, 1'b0}; {1'_, Ah} <- {Ah, Ad[0]}
$(@@sl1m Ah, L1); L1:Ad;
Z Al Lzn;
Lp:$(@@sl1m Al, Aend);
Lzn:Inc Al Ln; Dec Al Lp;
Ln:Dec Al; Inc Ad; Inc Ah; $(@@sl1m Al, Aend);

@@sl1c Ad, Al, Aend // {Ad, Al} <- {(w-1)'b0, Al, 1'd0}
L1:Ad;
Z Al Lzn;
Lp:$(@@sl1m Al, Aend);
Lzn:Inc Al Ln; Dec Al Lp;
Ln:Dec Al; Inc Ad; $(@@sl1m Al, Aend);


@@srl1 Ad, As, Aend // {Ad, 1'_} <- {1'b0, As}
As Z (- L2); L2:Ad; Z Ad (- L4); L4:Z Ad (- L5); L5:Z Z Aend;

@@srl1m A, Aend // {A, 1'd_} <- {1'd0, A}
A Z (- L2); L2:Z A (- L3); L3:Z Z Aend;

@@sl1 Ad, As, Aend
As Z; Ad; Z Ad; Z Ad; Z Z Aend;

@@sl1m A, Aend // {1'd_, A} <- {A, 1'd0}
A Z; Z A; Z Z Aend;

@@jnzp A, An, Az, Ap // goto An if A < 0, Az if A = 0, Ap if A > 0
Z A Lzn; Z Z Ap;
Lzn:Inc A Ln; Dec A Az;
Ln:Dec A An;

@@jezo A, Ae, Az, Ao // goto Az if zero, Ae if A is even, Ao if A is odd.
Z A (- Lzo); Z Z Ae;
Lzo:Inc A (- Lo); Lz:Min A (- Az);
Lo:Min A (- Ao);

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

@@multuSubO Hi, Lo, Rs, Rt, T0, T1, T2, T3, End
Lo; Rs T1;
CW T0;
Loop:Inc T0 LBody;
LFinish:T0; Rs; T1 Rs; T1; T3 T3 End;
LBody:$(@@sl1d Hi, Lo, LBody2);
LBody2:$(@@sl1c T3, Rs, LBody3);
LBody3:Z T3 Loop;
$(@@addc Hi, Lo, Rt, T2, Loop);

@@multuSub Hi, Lo, Rs, Rt, TRth, T2, Ts, Tt, End
Hi; Lo; Rs Ts; Rt Tt;
Loop:$(@@jezo Rs, LBodyE, LFinish, LBodyO);
LFinish:Ts Rs; Ts; Rt; Tt Rt; TRth; Tt Tt End;
LBodyO:$(@@addc Hi, Lo, Rt, T2, LBodyO1);
LBodyO1:TRth Z; Z Hi; Z;
LBodyE:$(@@sl1d TRth, Rt, LBodyE1);
LBodyE1:$(@@srl1m Rs, Loop);

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

