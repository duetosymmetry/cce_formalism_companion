(* ::Package:: *)

(* ::Input:: *)
(*SetOptions[$FrontEndSession, NotebookAutoSave -> True]*)
(*NotebookSave[]*)


(* ::Section:: *)
(*Calculation set-up*)


(* ::Subsection:: *)
(*xTensor initialization*)


(* ::Subsubsection::Closed:: *)
(*Import Calls*)


(* ::Input:: *)
(*<<xAct`xTensor`*)


(* ::Input:: *)
(*<<xAct`xCoba`*)


(* ::Input:: *)
(*<<xAct`xPert`*)


(* ::Input:: *)
(*<<xAct`TexAct`*)


(* ::Subsubsection::Closed:: *)
(*Output settings*)


(* ::Input:: *)
(*Unprotect[IndexForm];*)
(*IndexForm[LI[x_]]:="("<>ToString[x]<>")";*)
(*Protect[IndexForm];*)


(* ::Input:: *)
(*$PrePrint=ScreenDollarIndices;*)


(* ::Input:: *)
(*$DefInfoQ=False;*)


(* ::Subsection:: *)
(*Custom tensor utilities*)


(* ::Subsubsection::Closed:: *)
(*Definitions of equation diagnostic utilities*)


(* ::Input:: *)
(*RemoveIndependentOf[form_][expr_Plus]:=RemoveIndependentOf[form]/@expr;*)
(*RemoveIndependentOf[form_][expr_]/;FreeQ[expr,form]:=0;*)
(*RemoveIndependentOf[form_][expr_]:=expr;*)


(* ::Subsubsection::Closed:: *)
(*Definitions of Tensor utilities*)


(* ::Input:: *)
(*GenerateNewDummies[expr_]/;Head@expr===Plus:=GenerateNewDummies/@expr;*)


(* ::Input:: *)
(*GenerateNewDummies[expr_]/;!(Head@expr===Plus):=( *)
(*Module[{DummyList,ii,TempExpr},*)
(*	DummyList=UpIndex/@(IndicesOf[Dummy][expr]/.IndexList->List);*)
(*	TempExpr=expr;*)
(*	For[ii=1,ii<=Length[DummyList],ii++,*)
(*		If[BIndexQ[DummyList[[ii]]],*)
(*			With[{ind={Unique[DummyList[[ii]][[1]]],DummyList[[ii]][[2]]}},*)
(*				TempExpr=TempExpr//.{DummyList[[ii]]:>ind,-DummyList[[ii]]:>-ind}];,*)
(*			With[{ind=Unique[DummyList[[ii]]]},*)
(*				TempExpr=TempExpr//.{DummyList[[ii]]:>ind,-DummyList[[ii]]:>-ind};*)
(*			];*)
(*		];*)
(*	];*)
(*Return[TempExpr];*)
(*]);*)


(* ::Subsubsection::Closed:: *)
(*Rule generation*)


(* ::Text:: *)
(*Unfortunately, the more general xTensor utilities that would do this sort of thing generate so many rules that the resulting notebook really drags, so we need to do a more specialized thing here.*)


(* ::Input:: *)
(*IsSymmetric[expr_,ind1_,ind2_]:=( *)
(*Module[{swap},expr//.{ind1->swap}//.{ind2->ind1}//.{swap->ind2}]===expr);*)


(* ::Input:: *)
(*CreateContractionAutoRules[metric_,list_,{ind1_,ind2_},{ind1Pattern_,ind2Pattern_},OptionsPattern[]]:=*)
(*{{(Evaluate@Head[#1[[1]]])/:Evaluate[ContractMetric[metric[-ind1,-ind2] #1]/.*)
(*{ind1->ind1Pattern, ind2->ind2Pattern}]:= #2},*)
(* If[Not@IsSymmetric[#1,ind1,ind2],{(Evaluate@Head[#1[[1]]])/:Evaluate[ContractMetric[metric[-ind2,-ind1] #1]/.*)
(*{ind1->ind1Pattern, ind2->ind2Pattern}]:= #2},{}],*)
(* {metric/:Evaluate[metric[-ind1,-ind2]#1/.*)
(*{ind1->ind1Pattern, ind2->ind2Pattern}]:= #2},*)
(* If[Not@IsSymmetric[#1,ind1,ind2],{metric/:Evaluate[metric[-ind2,-ind1]#1/.*)
(*{ind1->ind1Pattern, ind2->ind2Pattern}]:= #2},{}]*)
(*}&@@@list;*)


(* ::Subsection:: *)
(*Bondi-Sachs Coordinate definitions and rules*)


(* ::Subsubsection::Closed:: *)
(*xTensor object definitions*)


(* ::Text:: *)
(*4-dimensional indices and metric*)


(* ::Input:: *)
(*DefManifold[M4,4,IndexRange[a,f]];*)


(* ::Input:: *)
(*DefMetric[-1,met[-a,-b],CD,PrintAs->"g"];*)


(* ::Text:: *)
(*2-dimensional indices and metric*)


(* ::Input:: *)
(*DefManifold[M2,2,{A,B,C,D,F,G}];*)
(*PrintAs[A]^="A";PrintAs[B]^="B";*)
(*Unprotect[C];Unprotect[D];*)
(*PrintAs[C]^="C";PrintAs[D]^="D";*)
(*Protect[C];Protect[D];*)
(*PrintAs[F]^="F";PrintAs[G]^="G";*)


(* ::Text:: *)
(*The xTensor system will treat this as the 'true' metric of the 2-dimensional tangent space *)
(*and raise/lower indices with it. This is what we want and is the standard notation for papers in this subject.*)


(* ::Input:: *)
(*DefMetric[1,sphmet2[-A,-B],sphd,PrintAs->"q"];*)


(* ::Text:: *)
(*The xTensor system will treat this as an additional quantity with formulaically defined derivatives and curvature, but not use it for canonicallization*)


(* ::Input:: *)
(*DefMetric[1,met2[-A,-B],cd,PrintAs->"h"];*)


(* ::Subsubsection::Closed:: *)
(*Bondi-Sachs component definitions*)


(* ::Text:: *)
(*'up' index basis vectors for the u and r coordinates.*)


(* ::Input:: *)
(*DefTensor[uv[a],{M4}];*)
(*DefTensor[rv[a],{M4}];*)


(* ::Text:: *)
(*'down' index basis vectors for the u and r coordinates*)


(* ::Input:: *)
(*DefTensor[rd[a],{M4}];*)
(*DefTensor[ud[b],{M4}];*)


(* ::Text:: *)
(*Angular basis vectors:*)


(* ::Input:: *)
(*DefTensor[basv[a,A],{M2,M4},PrintAs->"e"];*)
(*DefTensor[basd[a,A],{M2,M4},PrintAs->"e"];*)


(* ::Text:: *)
(*The angular null vectors; note that the 'v' indicates the 4-component version and without represents *)
(*the 2-component version.*)


(* ::Input:: *)
(*DefTensor[qv4[a],{M4},PrintAs->"q"];*)
(*DefTensor[qbv4[a],{M4},PrintAs->"\!\(\*OverscriptBox[\(q\), \(_\)]\)"];*)
(*DefTensor[qd4[a],{M2,M4},PrintAs->"q"];*)
(*DefTensor[qbd4[a],{M2,M4},PrintAs->"\!\(\*OverscriptBox[\(q\), \(_\)]\)"];*)


(* ::Input:: *)
(*DefTensor[q[A],{M2,M4},PrintAs->"q"];*)
(*DefTensor[qb[A],{M2,M4},PrintAs->"\!\(\*OverscriptBox[\(q\), \(_\)]\)"];*)


(* ::Text:: *)
(*The angular connection coefficient*)


(* ::Input:: *)
(*DefTensor[\[CapitalTheta][],{M2, M4}];*)
(*DefTensor[\[CapitalTheta]b[],{M2, M4},PrintAs->"\!\(\*OverscriptBox[\(\[CapitalTheta]\), \(_\)]\)"];*)


(* ::Text:: *)
(*Tensor representing the Bondi-Sachs radial coordinate*)


(* ::Input:: *)
(*DefTensor[r[],M4,PrintAs->"r"];*)


(* ::Text:: *)
(*Bondi-Sachs metric coefficient scalars*)


(* ::Input:: *)
(*DefTensor[be[],{M4},PrintAs->"\[Beta]"];*)
(*DefTensor[V[],M4,PrintAs->"V"];*)


(* ::Text:: *)
(*Bondi-Sachs angular component tensors*)


(* ::Input:: *)
(*DefTensor[h[A,B],{M2,M4}];*)
(*DefTensor[U[A],M4,PrintAs->"U"];*)


(* ::Input:: *)
(*DefTensor[Q[A],M4];*)


(* ::Text:: *)
(*Spin-weighted scalars for angular components*)


(* ::Input:: *)
(*DefTensor[Uq[],{M4},PrintAs->"\!\(\*SubscriptBox[\(U\), \(q\)]\)"];*)
(*DefTensor[Uqb[],{M4},PrintAs->"\!\(\*SubscriptBox[\(U\), OverscriptBox[\(q\), \(_\)]]\)"];*)


(* ::Input:: *)
(*DefTensor[Qq[],{M4},PrintAs->"\!\(\*SubscriptBox[\(Q\), \(q\)]\)"];*)
(*DefTensor[Qqb[],{M4},PrintAs->"\!\(\*SubscriptBox[\(Q\), OverscriptBox[\(q\), \(_\)]]\)"];*)


(* ::Input:: *)
(*DefTensor[J[],{M4}];*)
(*DefTensor[Jb[],{M4},PrintAs->"\!\(\*OverscriptBox[\(J\), \(_\)]\)"];*)


(* ::Input:: *)
(*DefTensor[K[],{M4}];*)
(*DefTensor[OnePlusK[],{M4},PrintAs->"(1+K)"];*)


(* ::Subsubsection::Closed:: *)
(*L AT EX rendering rules for spin-weighted scalars*)


(* ::Input:: *)
(*Tex[Jb[]]:="\\bar{J}"*)


(* ::Input:: *)
(*Tex[Qq[]]:="Q";*)
(*Tex[Qqb[]]:="\\bar{Q}"*)


(* ::Input:: *)
(*Tex[Uq[]]:="U";*)
(*Tex[Uqb[]]:="\\bar{U}";*)


(* ::Subsubsection::Closed:: *)
(*Automatic rules*)


(* ::Text:: *)
(*definition of Bondi metric quantities*)


(* ::Text:: *)
(*Contraction of basis vectors : 'up' versions, which represent Subscript[e, a]^\[Alpha], and are distinct tensors from the the 'down' versions below Subscript[e^a, \[Alpha]]. Each one has the (somewhat trivial) property in the Bondi coordinates of being the identity.*)


(* ::Text:: *)
(*Contraction of 'up' vectors with metric gives the components of the down-index metric:*)


(* ::Input:: *)
(*CreateContractionAutoRules[met, #, {a,b},{a_,b_}]&@*)
(*{{uv[a]uv[b], Module[{A,B},-Exp[2 be[]]*V[]/r[] + r[]^2 met2[-A,-B]U[A]U[B]]},*)
(* {uv[a]rv[b],-Exp[2be[]]},*)
(* {uv[a]qv4[b],Module[{A,B},-r[]^2 met2[-A, -B]U[A]q[B]]},*)
(* {uv[a]qbv4[b],Module[{A,B},-r[]^2 met2[-A, -B]U[A]qb[B]]},*)
(* {uv[a]basv[b, B_],Module[{C},-r[]^2 met2[-A, -C]sphmet2[B,C]U[A]]},*)
(* {rv[a]rv[b],0},*)
(* {rv[a]qv4[b],0},*)
(* {rv[a]qbv4[b],0},*)
(* {rv[a]basv[b, B_],0},*)
(* {qv4[a]qv4[b],Module[{A,B},r[]^2 met2[-A, -B] q[A] q[B]]},*)
(* {qv4[a]qbv4[b],Module[{A,B},r[]^2 met2[-A, -B] q[A] qb[B]]},*)
(* {qbv4[a]qbv4[b],Module[{A,B},r[]^2 met2[-A, -B] qb[A] qb[B]]},*)
(* {basv[a, A_]basv[b, B_], r[]^2 met2[A, B]}*)
(*};*)


(* ::Text:: *)
(*Contraction of 'down' vectors with *)


(* ::Input:: *)
(*CreateContractionAutoRules[met, #, {a,b},{a_,b_}]&@*)
(*{{ud[a]ud[b],0},*)
(* {ud[a]rd[b],-Exp[-2 be[]]},*)
(* {ud[a]qd4[b],0},*)
(* {ud[a]qbd4[b],0},*)
(* {ud[a]basd[b, B_],0},*)
(* {rd[a]rd[b],V[]Exp[-2 be[]] / r[]},*)
(* {rd[a]qd4[b],Module[{B},-q[-B] U[B] Exp[-2 be[]]]},*)
(* {rd[a]qbd4[b],Module[{B},-qb[-B] U[B] Exp[-2 be[]]]},*)
(* {rd[a]basd[b, B_],-U[B] Exp[-2 be[]]},*)
(* {qd4[a]qd4[b],Module[{A,B},q[-A] q[-B] met2[A, B] / r[]^2]},*)
(* {qd4[a]qbd4[b],Module[{A,B},q[-A] qb[-B] met2[A, B] / r[]^2]},*)
(* {qbd4[a]qbd4[b],Module[{A,B},qb[-A] qb[-B] met2[A, B] / r[]^2]},*)
(* {basd[a, A_]basd[b, B_], met2[A, B] / r[]^2}*)
(*};*)


(* ::Text:: *)
(*Orthonormality of the Bondi-Sachs basis vectors*)


(* ::Input:: *)
(*CreateContractionAutoRules[met, #, {a, b}, {a_,b_}]&@*)
(*{{uv[a]ud[b],      1},*)
(* {uv[a]rd[b],      0},*)
(* {uv[a]basd[b, B_], 0},*)
(* {uv[a]qd4[b],     0},*)
(* {uv[a]qbd4[b],    0}*)
(* ,*)
(* {rv[a]ud[b],      0},*)
(* {rv[a]rd[b],      1},*)
(* {rv[a]basd[b, B_], 0},*)
(* {rv[a]qd4[b],     0},*)
(* {rv[a]qbd4[b],    0}*)
(* ,*)
(* {qv4[a]ud[b],      0},*)
(* {qv4[a]rd[b],      0},*)
(* {qv4[a]basd[b, B_], q[B]},*)
(* {qv4[a]qd4[b],     0},*)
(* {qv4[a]qbd4[b],    2}*)
(* ,*)
(* {qbv4[a]ud[b],      0},*)
(* {qbv4[a]rd[b],      0},*)
(* {qbv4[a]basd[b, B_], qb[B]},*)
(* {qbv4[a]qd4[b],     2},*)
(* {qbv4[a]qbd4[b],    0}*)
(* ,*)
(* {basv[a, A_]ud[b],      0},*)
(* {basv[a, A_]rd[b],      0},*)
(* {basv[a, A_]basd[b, B_], sphmet2[A, B]},*)
(* {basv[a, A_]qd4[b],     q[A]},*)
(* {basv[a, A_]qbd4[b],    qb[A]}*)
(*};*)


(* ::Text:: *)
(*Using the identity property in Bondi-Sachs to elimiinate PD's of basis vectors*)
(*Note: q's don't get a similar identity because they are typically defined with angular dependence*)


(* ::Input:: *)
(*rv/:PD[_][rv[a_?UpIndexQ]]:=0;*)
(*uv/:PD[_][uv[a_?UpIndexQ]]:=0;*)
(*rd/:PD[_][rd[a_?DownIndexQ]]:=0;*)
(*ud/:PD[_][ud[a_?DownIndexQ]]:=0;*)
(*basv/:PD[_][basv[a_?UpIndexQ,b_?DownIndexQ]]:=0;*)
(*basd/:PD[_][basd[a_?DownIndexQ,b_?UpIndexQ]]:=0;*)


(* ::Text:: *)
(*Derivative of Bondi-Sachs radius*)


(* ::Input:: *)
(*r/:PD[a_][r[]]:=rd[a];*)


(* ::Text:: *)
(*Angular null vector identities*)


(* ::Input:: *)
(*CreateContractionAutoRules[sphmet2, #, {A, B},{A_, B_}]&@*)
(*{{q[A]q[B],   0},*)
(* {q[A]qb[B],  2},*)
(* {qb[A]qb[B], 0}*)
(*};*)


(* ::Input:: *)
(*rv/:rv[a_]PD[b_][*)
(*((q|qb)[_]|PD[_][(q|qb)[_]] )]/;a==ChangeIndex[b]:=0;*)
(*uv/:uv[a_]PD[b_][*)
(*((q|qb)[_]|PD[_][(q|qb)[_?UpIndexQ]])]/;a==ChangeIndex[b]:=0;*)


(* ::Text:: *)
(*Angular metric simplification rules*)


(* ::Input:: *)
(*met2/:met2[A_,B_]*(met2[-B_,-C_]|met2[-C_,-B_]):=sphmet2[A,-C];*)
(*met2/:met2[B_,A_]*(met2[-B_,-C_]|met2[-C_,-B_]):=sphmet2[A,-C];*)


(* ::Text:: *)
(*Simplification rules for K factors*)


(* ::Input:: *)
(*Unprotect[Sqrt];*)
(*Unprotect[Power];*)
(*Unprotect[Times];*)
(*Sqrt[K[]^2]:=K[];*)
(*(K[]^2)^(1/2):=K[];*)
(*Times/:Sqrt[1+K[]]*(Sqrt[1/(1 + K[])]):=1;*)
(*(K[]^2)^(3/2):=K[]^3;*)
(*Protect[Times];*)
(*Protect[Sqrt];*)
(*Protect[Power];*)


(* ::Subsubsection::Closed:: *)
(*Metric component substitution rules*)


(* ::Text:: *)
(*Metric value in terms of basis vectors and Bondi-Sachs components*)


(* ::Input:: *)
(*MetricValues={*)
(*met[a_?UpIndexQ,b_?UpIndexQ]*)
(*	:>Module[{A,B},( -(Exp[-2be[]]) (uv[a]rv[b] + uv[b]rv[a]) *)
(*					  + Exp[-2 be[]]*(V[]/r[])(rv[a]rv[b]) *)
(*					  + -Exp[-2 be[]] U[A](rv[a]basv[b,-A]+ basv[a,-A]rv[b])*)
(*					  + (1/(r[]^2))(met2[A,B]basv[a,-A]basv[b,-B]))],met[a_?DownIndexQ,b_?DownIndexQ]*)
(*	:>Module[{A,B},(- (V[]/r[])Exp[2 be[]] *)
(*					 + r[]^2 met2[-A,-B] U[A] U[B]) ud[a] ud[b]*)
(*					 - Exp[2 be[]](ud[a]rd[b] + rd[a]ud[b])*)
(*					 -r[]^2 U[A] met2[-A,-B]*)
(*				(ud[a] basd[b,B] + basd[a,B]ud[b]) *)
(*					 + r[]^2 met2[-A,-B] basd[a,A]basd[b,B]]};*)


(* ::Text:: *)
(*Lower the index of a partial and substitute the metric components*)


(* ::Input:: *)
(*LowerPDIndices:={*)
(*PD[a_?UpIndexQ][exp_]*)
(*:>(Module[{b},met[a,b]PD[-b][exp]]/.MetricValues)};*)


(* ::Text:: *)
(*Substitute the dyad decomposition of the angular metric*)


(* ::Input:: *)
(*met2toq={*)
(*met2[A_?DownIndexQ,B_?DownIndexQ]*)
(*:>(1/2)(J[]qb[A]qb[B] + Jb[]q[A]q[B]*)
(*			+ K[](q[A]qb[B] + qb[A]q[B])),*)
(*met2[A_?UpIndexQ,B_?UpIndexQ]*)
(*:>(1/2)(-J[]qb[A]qb[B] - Jb[]q[A]q[B] *)
(*		+ K[](q[A]qb[B] + qb[A]q[B]))};*)


(* ::Text:: *)
(*Substitute the dyad decomposition of the angular metric, in terms of only J (eliminating K)*)


(* ::Input:: *)
(*met2toqJ={*)
(*met2[A_?DownIndexQ,B_?DownIndexQ]*)
(*:>(1/2)(J[]qb[A]qb[B] + Jb[]q[A]q[B] *)
(*		+ Sqrt[1 + J[]Jb[]](q[A]qb[B] + qb[A]q[B])),*)
(*met2[A_?UpIndexQ,B_?UpIndexQ]*)
(*:>(1/2)(-J[]qb[A]qb[B] - Jb[]q[A]q[B] *)
(*		+ Sqrt[1 + J[]Jb[]](q[A]qb[B] + qb[A]q[B]))};*)


(* ::Text:: *)
(*Dyad decomposition for the U^A angular shift part of the metric*)


(* ::Input:: *)
(*Utoq={U[A_]:>((1/2)qb[A]Uq[] + (1/2)q[A]Uqb[])};*)


(* ::Text:: *)
(*Dyad decomposition for the Q^A radial derivative of the angular shift part of the metric*)


(* ::Input:: *)
(*Qtoq={Q[A_]->(1/2)(qb[A]Qq[] + q[A]Qqb[])};*)


(* ::Text:: *)
(*Identity for K given by assuming the determinant identity for the angular part of the metric*)


(* ::Input:: *)
(*KtoJ={K[]->Sqrt[1 + J[]Jb[]]};*)


(* ::Text:: *)
(*For more easily substituting the angular derivatives*)


(* ::Input:: *)
(*rv/:rv[a_]PD[-a_][Christoffelsphd[___]]:=0;*)
(*uv/:uv[a_]PD[-a_][Christoffelsphd[___]]:=0;*)


(* ::Input:: *)
(*ConvertToAngularSphd[expression_]:=( *)
(*ChangeCovD[expression*)
(*	//.{basv[a_,A_]PD[b_][PD[-a_][exp_]]:>PD[b][PD[A][exp]],*)
(*		basv[a_,A_]PD[-a_][exp_]:>PD[A][exp]}*)
(*,PD,sphd]\*)
(*//Expand//ToCanonical[#,UseMetricOnVBundle->None]&);*)


(* ::Input:: *)
(*sphdChristoffelToPDChristoffel[expr_]:=( *)
(*expr//.{sphd[A_][Christoffelsphd[inds__]]*)
(*		:>ChangeCovD[sphd[A][Christoffelsphd[inds]],sphd,PD]});*)


(* ::Subsubsection::Closed:: *)
(*Christoffel components*)


(* ::Text:: *)
(*Subscript[\[CapitalGamma]^c, r r]:*)


(* ::Input:: *)
(*(rv[a]rv[b]ChristoffelCD[c,-a,-b]//ChristoffelToGradMetric)/.MetricValues//Expand//ToCanonical*)


(* ::Text:: *)
(*Subscript[\[CapitalGamma]^u, r A]:*)


(* ::Input:: *)
(*(ud[-c]rv[a]basv[b,-B]ChristoffelCD[c,-a,-b]//ChristoffelToGradMetric)/.MetricValues//Expand//ToCanonical*)


(* ::Text:: *)
(*Subscript[\[CapitalGamma]^B, r A]:*)


(* ::Input:: *)
(*(basd[-c,C]rv[a]basv[b,-B]ChristoffelCD[c,-a,-b]//ChristoffelToGradMetric)/.MetricValues//Expand//ToCanonical*)


(* ::Text:: *)
(*g^AB Subscript[\[CapitalGamma]^u, AB]:*)


(* ::Input:: *)
(*((met[e,d]basd[-e,A]basd[-d,B]basv[a,-A]basv[b,-B]ChristoffelCD[-a,-b,c]ud[-c])//ChristoffelToGradMetric)/.MetricValues//Expand*)


(* ::Subsubsection::Closed:: *)
(*Derivative substitution rules*)


(* ::Text:: *)
(*partials in the Bondi-like coordinate directions*)


(* ::Input:: *)
(*DefInertHead[PDr,PrintAs->"\!\(\*SubscriptBox[\(\[PartialD]\), \(r\)]\)"];*)
(*DefInertHead[PDu,PrintAs->"\!\(\*SubscriptBox[\(\[PartialD]\), \(u\)]\)"];*)


(* ::Text:: *)
(*Substitution for the Bondi-like partials*)


(* ::Input:: *)
(*PDsToScalarDerivatives={*)
(*rv[a_]PD[-a_][expr_]:>PDr[expr],*)
(*uv[a_]PD[-a_][expr_]:>PDu[expr],*)
(*rv[a_]PDr[PD[-a_][expr_]]:>PDr[PDr[expr]],*)
(*rv[a_]PDu[PD[-a_][expr_]]:>PDu[PDr[expr]],*)
(*uv[a_]PDr[PD[-a_][expr_]]:>PDu[PDr[expr]],*)
(*uv[a_]PDu[PD[-a_][expr_]]:>PDu[PDu[expr]]};*)


(* ::Input:: *)
(*DefInertHead[Eth,PrintAs->"\[PartialD]"];*)
(*DefInertHead[Ethbar,PrintAs->"\!\(\*OverscriptBox[\(\[PartialD]\), \(_\)]\)"];*)


(* ::Input:: *)
(*ScalarPDList={PDr,PDu,Eth,Ethbar};*)


(* ::Text:: *)
(*Substitution rules for converting any radial derivatives of U to Q, requires scalar derivatives.*)


(* ::Input:: *)
(*drUtoQ={*)
(*PDr[Uq[]]:>Module[{A,B},qv[-A]met2[A,B]Q[-B]Exp[2 be[]]/(r[]^2)],*)
(*PDr[der_[Uq[]]]/;MemberQ[ScalarPDList,der]:>Module[{A,B},der[q[-A]met2[A,B]Q[-B]Exp[2 be[]]/(r[]^2)]],*)
(*PDr[Uqb[]]:>Module[{A,B},qbv[-A]met2[A,B]Q[-B]Exp[2 be[]]/(r[]^2)],*)
(*PDr[der_[Uqb[]]]/;MemberQ[ScalarPDList,der]:>Module[{A,B},der[qbv[-A]met2[A,B]Q[-B]Exp[2 be[]]/(r[]^2)]]};*)


(* ::Input:: *)
(*drUtoQ={*)
(*PDr[Uq[]]*)
(*:>Scalar[Module[{A,B},q[-A]met2[A,B]Q[-B]Exp[2 be[]]/(r[]^2)]],*)
(*PDr[der_[Uq[]]]/;MemberQ[ScalarPDList,der]*)
(*:>Scalar[Module[{A,B},der[q[-A]met2[A,B]Q[-B]Exp[2 be[]]/(r[]^2)]]],*)
(*PDr[Uqb[]]*)
(*:>Scalar[Module[{A,B},qb[-A]met2[A,B]Q[-B]Exp[2 be[]]/(r[]^2)]],*)
(*PDr[der_[Uqb[]]]/;MemberQ[ScalarPDList,der]*)
(*:>Scalar[Module[{A,B},der[qb[-A]met2[A,B]Q[-B]Exp[2 be[]]/(r[]^2)]]],*)
(*rv[a_]PD[b_][U[A_]]/;a==ChangeIndex[b]*)
(*:>Module[{B},met2[A,B]Q[-B]Exp[2 be[]]/(r[]^2)],*)
(*rv[a_](sphd[c_][PD[b_][U[A_]]]|PD[b_][sphd[c_][U[A_]]])/;a==ChangeIndex[b]*)
(*:>Module[{B},sphd[c][met2[A,B]Q[-B]Exp[2 be[]]/(r[]^2)]],*)
(*rv[a_](PD[b_][PD[c_][U[A_]]]|PD[c_][PD[b_][U[A_]]])/;a==ChangeIndex[b]*)
(*:>Module[{B},PD[c][met2[A,B]Q[-B]Exp[2 be[]]/(r[]^2)]]};*)


(* ::Input:: *)
(*ScalarDerivLeibniz={*)
(*der_[Sqrt[exp_]]/;MemberQ[ScalarPDList,der]*)
(*:>(1/(2 Sqrt[exp]))*der[exp],*)
(*der_[Exp[exp_]]/;MemberQ[ScalarPDList,der]*)
(*:>Exp[exp]*der[exp],*)
(*der_[ex1_ + ex2_]/;MemberQ[ScalarPDList,der]*)
(*:>der[ex1] + der[ex2],*)
(*der_[ex1_*ex2_]/;MemberQ[ScalarPDList,der]*)
(*:>ex2*der[ex1] + ex1*der[ex2],*)
(*der_[ex1_/ex2_]/;MemberQ[ScalarPDList,der]*)
(*:>der[ex1]/ex2 - ex1 * der[ex2]/(ex2^2),*)
(*der_[ex1_^(-1/2)]/;MemberQ[ScalarPDList,der]*)
(*:>-(1/2)der[ex1]/(ex1^(3/2)),*)
(*der_[ex1_^(n_?NumericQ)]/;MemberQ[ScalarPDList,der]*)
(*:>n*der[ex1]*(ex1^((n-1))),*)
(*der_[ex1_?NumericQ]/;MemberQ[ScalarPDList,der]:>0};*)


(* ::Input:: *)
(*GenerateSphdSubstitutions[tens_,qFactorList_List]:=*)
(*{sphd[-A_][tens]:>(1/2)Plus@@( *)
(*MapThread[(qb[-A]Eth[#1]*#2+q[-A]Ethbar[#1]*#2)&,Transpose@qFactorList]),*)
(*sphd[-A_][sphd[-B_][tens]]:>(1/4)Plus@@( *)
(*MapThread[*)
(*(qb[-A]qb[-B]Eth[Eth[#1]]*#2 + qb[-A]q[-B]Eth[Ethbar[#1]]*#2*)
(*+q[-A]qb[-B]Ethbar[Eth[#1]]*#2+q[-A]q[-B]Ethbar[Ethbar[#1]]*#2)&,*)
(*Transpose@qFactorList])}//ExpandAll;*)


(* ::Input:: *)
(*sphdToEth=Flatten@MapThread[GenerateSphdSubstitutions,*)
(*Transpose@{*)
(*{be[],{{be[],1}}},*)
(*{V[], {{V[],1}}},*)
(*{U[A_], {{Uq[],(1/2)qb[A]},{Uqb[],(1/2)q[A]}}},*)
(*{Q[A_], {{Qq[],(1/2)qb[A]},{Qqb[],(1/2)q[A]}}},*)
(*{met2[-A_,-B_],*)
(*{{J[],(1/2)qb[-A]qb[-B]},{Jb[],(1/2)q[-A]q[-B]},*)
(*{Sqrt[1 + J[]Jb[]],(1/2)(q[-A]qb[-B] + qb[-A]q[-B])}}}*)
(*}];*)


(* ::Subsubsection::Closed:: *)
(*Derivative of basis automatic rules*)


(* ::Input:: *)
(*Eth[r[]]:=0;*)
(*Ethbar[r[]]:=0;*)


(* ::Input:: *)
(*PDu[r[]]:=0;*)


(* ::Input:: *)
(*sphd[_][r[]]:=0;*)


(* ::Input:: *)
(*PDr[r[]]:=1;*)


(* ::Input:: *)
(*PDu[PDr[exp_]]:=PDr[PDu[exp]];*)


(* ::Input:: *)
(*PDr[Eth[exp_]]:=Eth[PDr[exp]];*)
(*PDr[Ethbar[exp_]]:=Ethbar[PDr[exp]];*)


(* ::Input:: *)
(*Ethbar[Eth[J[]]]:=Eth[Ethbar[J[]]] + 4 J[];*)
(*Ethbar[Eth[be[]]]:=Eth[Ethbar[be[]]];*)
(*Ethbar[Eth[Jb[]]]:=Eth[Ethbar[Jb[]]] - 4 Jb[]*)


(* ::Subsubsection::Closed:: *)
(*L AT EX rendering rules for derivatives*)


(* ::Input:: *)
(*Tex[Eth[exp_]]:="\\dh ("<>Tex[exp]<>")";*)
(*Tex[Ethbar[exp_]]:="\\bar{\\dh} ("<>Tex[exp]<>")";*)
(*Tex[PDr[exp_]]:="\\partial_r ("<>Tex[exp]<>")";*)
(*Tex[PDu[exp_]]:="\\partial_u ("<>Tex[exp]<>")";*)


(* ::Subsubsection::Closed:: *)
(*Derivative of dyad automatic rules*)


(* ::Text:: *)
(*Standardize the derivatives using integration by parts so canonicalization is most effective.*)


(* ::Input:: *)
(*q/:q[A_]PD[-a_][q[-A_]]:=-q[-A]PD[-a][q[A]];*)
(*q/:q[A_]PD[-a_][qb[-A_]]:=-qb[-A]PD[-a][q[A]];*)
(*qb/:qb[A_]PD[-a_][q[-A_]]:=-q[-A]PD[-a][qb[A]];*)
(*qb/:qb[A_]PD[-a_][qb[-A_]]:=-qb[-A]PD[-a][qb[A]];*)


(* ::Input:: *)
(*q/:q[A_]sphd[-B_][q[-A_]]:=0;*)
(*q/:q[-A_]sphd[-B_][q[A_]]:=0;*)
(*q/:q[A_]sphd[-B_][qb[-A_]]:=-qb[-A]sphd[-B][q[A]];*)
(*qb/:qb[A_]sphd[-B_][q[-A_]]:=qb[-A]sphd[-B][q[A]];*)
(*qb/:qb[A_]sphd[-B_][qb[-A_]]:=0;*)
(*qb/:qb[-A_]sphd[-B_][qb[A_]]:=0;*)


(* ::Input:: *)
(*q/:q[A_]PD[-a_][PD[-b_][q[-A_]]]\*)
(*:=PD[-a][q[A]PD[-b][q[-A]]] - PD[-b][q[-A]]PD[-a][q[A]];*)
(*q/:q[A_]PD[-a_][PD[-b_][qb[-A_]]]\*)
(*:=PD[-a][q[A]PD[-b][qb[-A]]] - PD[-b][qb[-A]]PD[-a][q[A]];*)
(*qb/:qb[A_]PD[-a_][PD[-b_][q[-A_]]]\*)
(*:=PD[-a][qb[A]PD[-b][q[-A]]] - PD[-b][q[-A]]PD[-a][qb[A]];*)
(*qb/:qb[A_]PD[-a_][PD[-b_][qb[-A_]]]\*)
(*:=PD[-a][qb[A]PD[-b][qb[-A]]] - PD[-b][qb[-A]]PD[-a][qb[A]];*)


(* ::Input:: *)
(*q/:q[A_]sphd[-a_][sphd[-b_][q[-A_]]]\*)
(*:=sphd[-a][q[A]sphd[-b][q[-A]]] - sphd[-b][q[-A]]sphd[-a][q[A]];*)
(*q/:q[A_]sphd[-a_][sphd[-b_][qb[-A_]]]\*)
(*:=sphd[-a][q[A]sphd[-b][qb[-A]]] - sphd[-b][qb[-A]]sphd[-a][q[A]];*)
(*qb/:qb[A_]sphd[-a_][sphd[-b_][q[-A_]]]\*)
(*:=sphd[-a][qb[A]sphd[-b][q[-A]]] - sphd[-b][q[-A]]sphd[-a][qb[A]];*)
(*qb/:qb[A_]sphd[-a_][sphd[-b_][qb[-A_]]]\*)
(*:=sphd[-a][qb[A]sphd[-b][qb[-A]]] - sphd[-b][qb[-A]]sphd[-a][qb[A]];*)


(* ::Input:: *)
(*PDr[(q|qb)[_]]:=0;*)
(*PDr[sphd[_][(q|qb)[B_]]]:=0;*)
(*PDu[(q|qb)[_]]:=0;*)
(*PDu[sphd[_][(q|qb)[_]]]:=0;*)


(* ::Input:: *)
(*q/:q[A_]sphd[-A_][PDr[exp_]]:=PDr[q[A]sphd[-A][exp]/.sphdToEth];*)
(*qb/:qb[A_]sphd[-A_][PDr[exp_]]:=PDr[qb[A]sphd[-A][exp]/.sphdToEth];*)


(* ::Input:: *)
(*DeclareSphdReexpansion[expr_,{indices__},qForm_,qFactors_]:=*)
(*(sphd[A_][expr]:=Module[{indices},sphd[A][qForm * qFactors]];*)
(*sphd[A_][PDu[expr]]:=Module[{indices},PDu[sphd[A][qForm * qFactors]]];*)
(*sphd[A_][PDr[expr]]:=Module[{indices},PDr[sphd[A][qForm * qFactors]]];*)
(*sphd[A_][Eth[expr]]:=Module[{indices,A$1},sphd[A][q[A$1]sphd[-A$1][qForm] * qFactors]];*)
(*sphd[A_][Ethbar[expr]]:=Module[{indices,A$1},sphd[A][qb[A$1]sphd[-A$1][qForm] * qFactors]];*)
(*);*)


(* ::Text:: *)
(*For scalars, we don't want the versions that will cause infinite recursion.*)


(* ::Input:: *)
(*DeclareSphdReexpansion[expr_]:=*)
(*(sphd[A_][Eth[expr]]:=Module[{A$1},sphd[A][q[A$1]sphd[-A$1][expr]]];*)
(*sphd[A_][Ethbar[expr]]:=Module[{A$1},sphd[A][qb[A$1]sphd[-A$1][expr]]];*)
(*);*)


(* ::Input:: *)
(*DeclareSphdReexpansion[Sequence@@#]&/@{*)
(*{Uq[],{B},U[B],q[-B]},*)
(*{Uqb[],{B},U[B],qb[-B]},*)
(*{Qq[],{B},Q[-B],q[B]},*)
(*{Qqb[],{B},Q[-B],qb[B]},*)
(*{J[],{B,C},(1/2)met2[-B,-C],q[B]q[C]},*)
(*{Jb[],{B,C},(1/2)met2[-B,-C],qb[B]qb[C]},*)
(*{K[],{B,C},(1/2)met2[-B,-C],q[B]qb[C]},*)
(*{V[]},*)
(*{be[]}};*)


(* ::Subsubsection::Closed:: *)
(*Custom conjugation function*)


(* ::Input:: *)
(*DefInertHead[Conj]*)


(* ::Input:: *)
(*Conj[ex1_ + ex2_]:=Conj[ex1]+Conj[ex2];*)
(*Conj[ex1_ * ex2_]:=Conj[ex1]*Conj[ex2];*)
(*Conj[ex1_?NumericQ]:=ex1;*)
(*Conj[ex_^n_]:=Conj[ex]^Conj[n];*)
(*Conj[r[]]:=r[];*)
(*Conj[PDr[ex_]]:=PDr[Conj[ex]];*)
(*Conj[PDu[ex_]]:=PDu[Conj[ex]];*)
(*Conj[Eth[ex_]]:=Ethbar[Conj[ex]];*)
(*Conj[Ethbar[ex_]]:=Eth[Conj[ex]];*)
(*Conj[be[]]:=be[];*)
(*Conj[Qq[]]:=Qqb[];*)
(*Conj[Qqb[]]:=Qq[];*)
(*Conj[Uq[]]:=Uqb[];*)
(*Conj[Uqb[]]:=Uq[];*)
(*Conj[J[]]:=Jb[];*)
(*Conj[Jb[]]:=J[];*)
(*Conj[K[]]:=K[];*)
(*Conj[V[]]:=V[];*)
(*Conj[basv[ind__]]:=basv[ind];*)
(*Conj[q[A_]]:=qb[A];*)
(*Conj[qb[A_]]:=q[A];*)
(*Conj[sphd[A_][exp_]]:=sphd[A][Conj[exp]];*)
(*Conj[OnePlusK[]]:=OnePlusK[];*)
(*Conj[\[CapitalTheta][]]:=\[CapitalTheta]b[];*)
(*Conj[\[CapitalTheta]b[]]:=\[CapitalTheta][];*)


(* ::Subsubsection::Closed:: *)
(*Simplification utilities*)


(* ::Input:: *)
(*ListToPatternSet[list_List]:=Alternatives@@list;*)


(* ::Input:: *)
(*GatherFactors[GatherList_][expr_]:=*)
(*GatherBy[expr,Times@@Cases[#,ListToPatternSet[GatherList]]&];*)


(* ::Input:: *)
(*SimplifyWithTermSort[GatherList_List][expr_Plus]:=Plus@@(Plus@@@GatherFactors[GatherList][List@@expr]//FullSimplify);*)


(* ::Input:: *)
(*SimplifyWithTermSort[GatherFunction_][expr_Plus]:=Plus@@(Plus@@@GatherFunction[List@@expr]//FullSimplify);*)


(* ::Input:: *)
(*SimplifyWithTermSort[GatherFunction_,SimplifyFunction_][expr_Plus]:=Plus@@(Plus@@@GatherFunction[List@@expr]//SimplifyFunction);*)


(* ::Input:: *)
(*SimplifyWithTermSort[Gather_][expr_]:=expr;*)


(* ::Input:: *)
(*SimplifyWithTermSort[Gather_][expr_Equal]:=(SimplifyWithTermSort[Gather][expr[[1]]])==(SimplifyWithTermSort[Gather][expr[[2]]])*)


(* ::Input:: *)
(*GatherScalarDerivFactors[gatherExpression_]:=*)
(*GatherBy[gatherExpression,*)
(*#/.{*)
(*exp_*PD2_[PD1_[expr1_]]*)
(*		/;MemberQ[ScalarPDList,PD1]&&MemberQ[ScalarPDList,PD2]*)
(*			:>PD2[PD1[expr1]],*)
(*exp_*PD1_[expr1_]*PD2_[expr2_]*)
(*		/;MemberQ[ScalarPDList,PD1]&&MemberQ[ScalarPDList,PD2]*)
(*			:>PD1[expr1]PD2[expr2],*)
(*exp_*PD1_[expr1_]*)
(*/;MemberQ[ScalarPDList,PD1]:>PD1[expr1],*)
(*exp_*PD1_[expr1_]^2*)
(*/;MemberQ[ScalarPDList,PD1]:>PD1[expr1]^2}&];*)


(* ::Input:: *)
(*JtoKRule={J[]Jb[]:>K[]^2 - 1,J[]^2Jb[]^2:>(K[]^2 - 1)^2,J[]Jb[]^2:>(K[]^2-1)Jb[],J[]^2Jb[]^3:>(K[]^2-1)^2Jb[]};*)


(* ::Input:: *)
(*SimplifyOnePlusKFactor[expr_]:=(((expr//.JtoKRule//ExpandAll//FullSimplify)/.JtoKRule//FullSimplify//Expand)/.{K[]->OnePlusK[]-1}//Expand)/.{OnePlusK[]->1+K[]}*)


(* ::Section:: *)
(*Characteristic Einstein field equations*)


(* ::Subsection:: *)
(*Tensor forms*)


(* ::Subsubsection::Closed:: *)
(*Utilities*)


(* ::Input:: *)
(*RiemannRicciToPDMet[exp_]:=( *)
(*exp//ChangeCurvature//NoScalar//ChristoffelToGradMetric*)
(*//Expand//ToCanonical[#,UseMetricOnVBundle->None]&);*)


(* ::Text:: *)
(*We use the Ricci tensor because its easier to simplify below.*)


(* ::Input:: *)
(*ExpandedEFE=((1/2)RicciCD[-a,-b]//RiemannRicciToPDMet);*)


(* ::Input:: *)
(*SimplifyEFEComponent[expr_]:=(expr//ExpandAll)/.MetricValues//ExpandAll\*)
(*//ToCanonical[#,UseMetricOnVBundle->None]&//ConvertToAngularSphd\*)
(*//sphdChristoffelToPDChristoffel//ExpandAll//ToCanonical[#,UseMetricOnVBundle->None]&;*)


(* ::Subsubsection::Closed:: *)
(*Tensor hypersurface equations*)


(* ::Input:: *)
(*TensorHypersurfaceBeta=(-r[]/2*rv[a]rv[b]ExpandedEFE)//SimplifyEFEComponent*)


(* ::Input:: *)
(*TensorHypersurfaceQ=-4*r[]^2(rv[a]q[A]basv[b,-A]ExpandedEFE)//SimplifyEFEComponent*)


(* ::Input:: *)
(*TensorHypersurfaceU=( -q[-A]rv[a]PD[-a][U[A]]  *)
(*					+ q[-A](rv[a]PD[-a][U[A]]/.drUtoQ))*)


(* ::Input:: *)
(*TensorHypersurfaceW=\*)
(*(((met2[A,B]basv[a,-A]basv[b,-B]ExpandedEFE )//SimplifyEFEComponent)*)
(*+ (1/2)(Riccisphd[-A,-B]met2[A,B]) *)
(*- (1/2)((Riccisphd[-A,-B]//ChangeCurvature)met2[A,B])//ExpandAll//ToCanonical[#,UseMetricOnVBundle->None]&)*)


(* ::Input:: *)
(*TensorEvolution=\*)
(*(((-q[A]q[B]basv[a,-A]basv[b,-B] ExpandedEFE)//SimplifyEFEComponent)*)
(*-(1/2) (Riccisphd[-A,-B]q[A]q[B]) *)
(*+(1/2)((Riccisphd[-A,-B]//ChangeCurvature)q[A]q[B]))\*)
(*	//ExpandAll//ToCanonical[#,UseMetricOnVBundle->None]&*)


(* ::Subsubsection::Closed:: *)
(*Tensor constraint equations*)


(* ::Input:: *)
(*Constraint1=(rv[a]uv[b] ExpandedEFE)//SimplifyEFEComponent*)


(* ::Input:: *)
(*Constraint2=(uv[a]uv[b] ExpandedEFE)//SimplifyEFEComponent*)


(* ::Input:: *)
(*Constraint3=(uv[a]basv[b,-A]ExpandedEFE)//SimplifyEFEComponent*)


(* ::Subsection:: *)
(*Spin-weighted scalar forms*)


(* ::Subsubsection::Closed:: *)
(*Utilities*)


(* ::Input:: *)
(*SubRiccisphd=Riccisphd[-A_,-B_]:>(1/2)(q[-A]qb[-B] + q[-B]qb[-A]);*)


(* ::Input:: *)
(*TensorExpressionToSimplifiedSWScalar[expr_]:=( *)
(*((((expr/.SubRiccisphd//.sphdToEth//Expand)/.met2toqJ//Expand)*)
(*/.Utoq//.Qtoq//Expand)//. PDsToScalarDerivatives//.drUtoQ//NoScalar)*)
(*/.met2toqJ/.Qtoq//ExpandAll)//.ScalarDerivLeibniz//Expand\*)
(*	//ToCanonical//NoScalar//SimplifyWithTermSort[GatherScalarDerivFactors]*)


(* ::Input:: *)
(*CompareEquations[Eq1_Equal, Eq2_Equal]:=( *)
(*(((((Eq1[[1]]-Eq2[[1]]-Eq1[[2]]+Eq2[[2]])//Expand)*)
(*/.PDsToScalarDerivatives//Expand)/.KtoJ//.drUtoQ/.met2toqJ/.Qtoq//NoScalar//ExpandAll)*)
(*		//.ScalarDerivLeibniz//.drUtoQ/.met2toqJ/.Qtoq//NoScalar//ExpandAll)//.ScalarDerivLeibniz//Expand//Simplify//ReleaseHold//Expand)==0;*)


(* ::Subsubsection::Closed:: *)
(*Spin-weighted scalar hypersurface equations*)


(* ::Input:: *)
(*SWHypersurfaceBeta=PDr[be[]]==(TensorHypersurfaceBeta//TensorExpressionToSimplifiedSWScalar)+PDr[be[]]*)


(* ::Input:: *)
(*SWHypersurfaceQ=*)
(*PDr[Qq[]r[]^2]==((TensorHypersurfaceQ//TensorExpressionToSimplifiedSWScalar)*)
(*				+PDr[Qq[]r[]^2]//.ScalarDerivLeibniz)*)


(* ::Input:: *)
(*SWHypersurfaceU=PDr[Uq[]]==(((TensorHypersurfaceU/.Utoq/.Qtoq/.met2toqJ//Expand)/.PDsToScalarDerivatives) + PDr[Uq[]])*)


(* ::Input:: *)
(*SWHypersurfaceW=PDr[V[]]==(Exp[2be[]]*TensorHypersurfaceW//TensorExpressionToSimplifiedSWScalar) + PDr[V[]]*)


(* ::Input:: *)
(*SWEvolution=PDr[r[]PDu[J[]]]==((Exp[2be[]]/r[])(TensorEvolution//TensorExpressionToSimplifiedSWScalar)//Expand)+PDr[r[]PDu[J[]]]//.ScalarDerivLeibniz*)


(* ::Subsubsection::Closed:: *)
(*Extra scalar definitions for condensed forms*)


(* ::Input:: *)
(*DefTensor[scAQ[],{M4},PrintAs->"\!\(\*SubscriptBox[\(A\), \(Q\)]\)"];*)
(*Tex[scAQ[]]:="\\mathcal{A}_Q";*)
(*Tex[Conj[scAQ[]]]:="\\bar{\\mathcal{A}_Q}";*)


(* ::Input:: *)
(*DefTensor[scAW[],{M4},PrintAs->"\!\(\*SubscriptBox[\(A\), \(W\)]\)"];*)
(*Tex[scAW[]]:="\\mathcal{A}_W";*)
(*Tex[Conj[scAW[]]]:="\\bar{\\mathcal{A}_W}";*)


(* ::Input:: *)
(*DefTensor[scAJ[],{M4},PrintAs->"\!\(\*SubscriptBox[\(A\), \(J\)]\)"];*)
(*DefTensor[scBJ[],{M4},PrintAs->"\!\(\*SubscriptBox[\(B\), \(J\)]\)"];*)
(*DefTensor[scCJ[],{M4},PrintAs->"\!\(\*SubscriptBox[\(C\), \(J\)]\)"];*)
(*DefTensor[scDJ[],{M4},PrintAs->"\!\(\*SubscriptBox[\(D\), \(J\)]\)"];*)


(* ::Input:: *)
(*Tex[scAJ[]]:="\\mathcal{A}_J";*)
(*Tex[scBJ[]]:="\\mathcal{B}_J";*)
(*Tex[scCJ[]]:="\\mathcal{C}_J";*)
(*Tex[scDJ[]]:="\\mathcal{D}_J";*)


(* ::Input:: *)
(*SubSuplimentaryScalars={*)
(*scAQ[]->scAQValue,scAW[]->scAWValue,*)
(*scAJ[]->scAJValue, scBJ[]->scBJValue,*)
(*scCJ[]->scCJValue, scDJ[]->scDJValue};*)


(* ::Subsubsection::Closed:: *)
(*Condensed forms from manual simplification*)


(* ::Text:: *)
(*Beta equation*)


(* ::Input:: *)
(*SWHypersurfaceBetaCondensed=PDr[be[]] == (r[]/8)*(- (PDr[J[]Jb[]]^2)/(4(1 + J[]Jb[])) + PDr[J[]]PDr[Jb[]])*)


(* ::Text:: *)
(*Q equation, simplified using an additional scalar which appears multiple times*)


(* ::Input:: *)
(*SWHypersurfaceQCondensed = PDr[r[]^2 Qq[]]==2 r[]^4 PDr[Eth@be[]/r[]^2] -  r[]^2 (1/K[] PDr@Ethbar@J[]+ scAQ[] +  J[]/K[] Conj[scAQ[]])*)


(* ::Input:: *)
(*scAQValue=-1/2 Eth[Jb[]PDr@J[]] - 1/2 Eth@Jb[] PDr@J[] +  1/2 J[] PDr@Eth@Jb[] + 1/(4 K[]^2) (Eth[J[]Jb[]]PDr[J[]Jb[]])*)


(* ::Text:: *)
(*U equation is already as condensed as it gets*)


(* ::Input:: *)
(*SWHypersurfaceUCondensed=SWHypersurfaceU//Simplify*)


(* ::Text:: *)
(*W equation, simplified using an additional scalar which appears multiple times (reality condition)*)


(* ::Input:: *)
(*SWHypersurfaceWCondensed=PDr[V[]] == r[] (Eth[Uqb[]] + Ethbar[Uq[]])+ 1/4 r[]^2 (PDr[Eth[Uqb[]]] + PDr[Ethbar[Uq[]]]) + Exp[2 be[]]/2 (scAW[] + Conj[scAW[]])*)


(* ::Input:: *)
(*scAWValue=( *)
(*J[](Ethbar@be[])^2 + K[]Eth@be[]Ethbar@be[] - 1/(2K[]) Eth[J[]Jb[]]Ethbar[be[]]*)
(*+ J[] Ethbar@Ethbar@be[]- (1/8)Eth[Ethbar[J[]Jb[]]]/K[] *)
(*- (1/4)Jb[]Eth[Ethbar[J[]]]/K[] + ((1/2)*Ethbar@Ethbar@J[]- 1/(4K[]) Ethbar@J[] Eth@Jb[]) *)
(*+ 1/(8K[]^3) (Eth[J[]Jb[]]Ethbar[J[]Jb[]]) + (1/(2K[]) + K[]/2) *)
(*+ 1/4 (J[] Qqb[]^2 - K[] Qq[] Qqb[]) *)
(*- K[]Eth[Ethbar[be[]]] - 2K[] Eth[be[]] Ethbar[be[]]  + Eth@be[] Eth@Jb[])*)


(* ::Text:: *)
(*H equation, simplified using several additional scalars that each appear multiple times.*)


(* ::Input:: *)
(*SWEvolutionCondensed=( *)
(*PDr[PDu[r[]J[]]] *)
(*+ J[](PDu[r[]J[]] scDJ[] + Conj[PDu[r[]J[]] scDJ[]]) *)
(*	== Exp[2 be[]]/r[] ( *)
(*	Eth@Eth@be[] + (Eth@be[] - Qq[]/2)^2 - 1/2 Eth@Qq[] *)
(*	+ 1/(4K[]) Eth[J[](Qqb[] - 2 Ethbar@be[])] - 1/(4K[]) J[] Eth@Qqb[]*)
(*	- J[](scAJ[] + Conj[scAJ[]]) + (scCJ[] + J[]^2/K[]^2 Conj[scCJ[]]))*)
(*- 1/2 Uq[] PDr[r[]Ethbar@J[]] - 1/2 Eth[Uqb[](r[]PDr[J[]] + J[])] *)
(*- Eth@Uqb[] J[] - (1/2)Ethbar@Uq[] J[] - K[] Eth@Uq[] + 1/2 V[]PDr@PDr@J[]*)
(*+ PDr@J[] (1/2 PDr@V[] + 1/4 r[] K[]^2 (Ethbar@Uq[] - Eth@Uqb[]) *)
(*		- 1/2 r[] K[] ( 1/(K[]^2) Jb[] Eth@Uq[] - J[] Ethbar@Uqb[]) *)
(*		+ 1/(2r[]) V[])*)
(*+ PDr@Jb[] ((K[]/2 - 1/(2K[])) r[]  J[] Eth@Uq[] *)
(*			- 1/4 r[] J[]^2 (Ethbar@Uq[] - Eth@Uqb[]))*)
(*	+ J[](scBJ[] + Conj[scBJ[]]))*)


(* ::Input:: *)
(*scAJValue=(1/4 Eth@Eth@Jb[] *)
(*+ 1/2 Eth@Jb[](Eth@be[] - 1/2 Qq[] - (2 K[]^2 - 1)/(4K[]^3) Ethbar@J[] *)
(*				+J[]/(4K[]^3) Ethbar[J[]Jb[]])*)
(*- Jb[]/(4K[]) Eth@Ethbar@J[] (1 - 1/(4K[]^2)) - 1/(4K[])Eth@Ethbar@be[] *)
(*+ 1/(16 K[]^3) (J[] Hold@Ethbar@Eth@Jb[] - Eth[Ethbar[J[]Jb[]]]) *)
(*+3/(4K[])  -  1/(4K[]^3))*)


(* ::Input:: *)
(*scBJValue=( 1/(2r[]) PDr[V[]]*)
(*+V[]/4 (1/(4K[]^2) PDr[J[]Jb[]]^2-PDr[J[]]PDr[Jb[]])*)
(*-1/(4K[]) Jb[] r[] PDr[J[]Jb[]]Eth@Uq[]*)
(*-1/(8K[]^2) r[] Uq[] PDr[J[]Jb[]]Ethbar[J[]Jb[]]*)
(*+1/4 r[](Uqb[] Eth[J[]PDr[Jb[]]] -  Uqb[] J[] PDr[Eth[Jb[]]]*)
(*		+ PDr[Jb[]] Uq[] Ethbar@J[]))*)


(* ::Input:: *)
(*scCJValue=K[]/2 Ethbar@J[](Eth@be[] - Qq[]/2)*)


(* ::Input:: *)
(*scDJValue=1/2 (1/(2 K[]^2) Jb[] PDr[J[]Jb[]] - PDr[Jb[]])*)


(* ::Subsubsection::Closed:: *)
(*Verification of condensed forms*)


(* ::Input:: *)
(*CompareEquations[SWHypersurfaceBeta,SWHypersurfaceBetaCondensed]*)


(* ::Input:: *)
(*CompareEquations[SWHypersurfaceQ,SWHypersurfaceQCondensed/.SubSuplimentaryScalars]*)


(* ::Input:: *)
(*CompareEquations[SWHypersurfaceU,SWHypersurfaceUCondensed/.SubSuplimentaryScalars]*)


(* ::Input:: *)
(*scBJnValue[LI[2]]=(PDnu[R[]]/R[] scDJn[] PDy[J[]] - 1/4 W[] PDy@J[] PDy@Jb[] *)
(*				+ 1/16 W[]PDy[J[]Jb[]]^2/K[]^2)*)


(* ::Input:: *)
(*CompareEquations[SWHypersurfaceW,SWHypersurfaceWCondensed/.SubSuplimentaryScalars]*)


(* ::Input:: *)
(*CompareEquations[SWEvolution,SWEvolutionCondensed/.SubSuplimentaryScalars]*)


(* ::Subsubsection::Closed:: *)
(*Forms from PittNull papers*)


(* ::Input:: *)
(*SWHypersurfaceBetaPitt= PDr[be[]]==(r[]/8)(PDr[J[]]PDr[Jb[]] - PDr[K[]]^2)*)


(* ::Input:: *)
(*SWHypersurfaceQPitt=( *)
(*rv[a]PD[-a][r[]^2 Qq[]]==*)
(* 2 r[]^4 rv[a]PD[-a][ Eth[be[]]/r[]^2] *)
(*+r[]^2 (-K[] rv[a]PD[-a][(Eth[K[]] + Ethbar[J[]])] *)
(*		+ Eth[Jb[] rv[a]PD[-a][J[]]] + Ethbar[J[] rv[a]PD[-a][K[]]]*)
(*		-Ethbar[K[]]rv[a]PD[-a][J[]] *)
(*		+ (1/(2K[]^2))( *)
(*			Eth[Jb[]](rv[a]PD[-a][J[]] - J[]^2 rv[a]PD[-a][Jb[]]) *)
(*			+ Eth[J[]](rv[a]PD[-a][Jb[]] - Jb[]^2 rv[a]PD[-a][J[]]))))*)


(* ::Input:: *)
(*SWHypersurfaceUPitt=(PDr[Uq[]]== (Exp[2be[]]/r[]^2)(K[]Qq[]-J[]Qqb[]))*)


(* ::Text:: *)
(*PittNull W hypersurface equation*)


(* ::Input:: *)
(*RGomez = ( *)
(*2 K[] - Eth@Ethbar@K[] + 1/2 (Ethbar@Ethbar@J[] + Eth@Eth@Jb[]) *)
(*+ 1/(4 K[]) (Ethbar@Jb[] Eth@J[] - Ethbar@J[] Eth@Jb[]))*)


(* ::Input:: *)
(*SWHypersurfaceWPitt=( *)
(*PDr[V[]]==*)
(*(1/2)Exp[2be[]]RGomez -Exp[be[]] Eth@Ethbar[Exp[be[]]]*)
(*+1/4 r[]^-2 (Eth@PDr[r[]^4 Uqb[]] + Ethbar@PDr[r[]^4 Uq[]])*)
(*+ Exp[2 be[]] ((1 - K[]) (Eth@Ethbar@be[] + Eth@be[] Ethbar@be[])*)
(*			+ 1/2 (J[] (Ethbar@be[])^2 +  Jb[] (Eth@be[])^2) *)
(*			- 1/2 (Eth@be[] (Ethbar@K[] - Eth@Jb[])*)
(*				+Ethbar@be[] (Eth@K[] - Ethbar@J[]))*)
(*			+ 1/2 (J[] Ethbar@Ethbar@be[] + Jb[] Eth@Eth@be[]))*)
(*- Exp[-2 be[]] r[]^4/8 (2 K[] PDr@Uq[] PDr@Uqb[] + J[] (PDr@Uqb[])^2 + Jb[] (PDr@Uq[])^2))*)


(* ::Text:: *)
(*PittNull H (evolution) equation*)


(* ::Input:: *)
(*NJs[1] = -(Exp[2 be[]]/r[]) (K[] (Eth@J[] Ethbar@be[] + 2 Eth@K[] Eth@be[] - Ethbar@J[] Eth@be[]) + *)
(*						     J[] (Ethbar@J[] Ethbar@be[] - 2 Eth@K[] Ethbar@be[]) - Jb[] Eth@J[] Eth@be[]);*)
(*NJs[2] = -1/2 (Eth@J[] (r[] PDr@Uqb[] + 2 Uqb[]) + *)
(*			   Ethbar@J[] (r[] PDr@Uq[] + 2 Uq[]));*)
(*NJs[3] = ((1 - K[]) (r[] Eth@PDr@Uq[] + 2 Eth@Uq[]) - *)
(*               J[] (r[] Eth@PDr@Uqb[] + 2 Eth@Uqb[]));*)
(*NJs[4] = r[]^3/2 Exp[-2 be[]] (K[]^2 (PDr@Uq[])^2 + 2 J[] K[] PDr@Uq[] PDr@Uqb[] + J[]^2 (PDr@Uqb[])^2);*)
(*NJs[5] = -r[]/2 PDr@J[] (Eth@Uqb[] + Ethbar@Uq[]);*)
(*NJs[6] = r[] (1/2 (Uqb[] Eth@J[] + Uq[] Ethbar@J[]) (J[] PDr@Jb[] - Jb[] PDr@J[]) + *)
(*			  (J[] PDr@K[] - K[] PDr@J[]) Uqb[] Ethbar@J[] -*)
(*			   Uqb[] (Eth@PDr@J[] - 2 K[] Eth@K[] PDr@J[] +  2 J[] Eth@K[] PDr@K[]) - *)
(*               Uq[] (Ethbar@PDr@J[] - K[] Eth@Jb[] PDr@J[] + J[] Eth@Jb[] PDr@K[]));*)
(*NJs[7] =( r[] (PDr@J[] K[] - J[] PDr@K[]) *)
(*		(Uqb[] (Ethbar@J[] - Eth@K[]) + *)
(*         Uq[] (Ethbar@K[] - Eth@Jb[]) + *)
(*         K[] (Ethbar@Uq[] - Eth@Uqb[]) +*)
(*	    (J[] Ethbar@Uqb[] - Jb[] Eth@Uq[])));*)


(* ::Input:: *)
(*Ps[1] = r[]^2 (PDu@J[]/K[] (PDr@Jb[] K[] - Jb[] PDr@K[]) + *)
(*               PDu@Jb[]/K[] (PDr@J[] K[] - J[] PDr@K[])) - *)
(*        8 V[] PDr@be[];*)
(*Ps[2] = Exp[2 be[]] (-2 K[] (Eth@Ethbar@be[] + Ethbar@be[] Eth@be[])*)
(*					 - (Ethbar@be[] Eth@K[] + Eth@be[] Ethbar@K[])*)
(*					 + (J[] (Ethbar@Ethbar@be[] + (Ethbar@be[])^2) + *)
(*                        Jb[] (Eth@Eth@be[] + (Eth@be[])^2)) *)
(*					 + (Ethbar@J[] Ethbar@be[] + Eth@Jb[] Eth@be[]));*)
(*Ps[3] = r[]/2 ((r[] Ethbar@PDr@Uq[] + 2 Ethbar@Uq[]) + (r[] Eth@PDr@Uqb[] + 2 Eth@Uqb[]));*)
(*Ps[4] = -r[]^4/4 Exp[-2 be[]] (2 K[] PDr@Uq[] PDr@Uqb[] + J[] (PDr@Uqb[])^2 + Jb[] (PDr@Uq[])^2);*)


(* ::Input:: *)
(*SWEvolutionPitt =( *)
(*  PDr[PDu[r[] J[]]]== (1/2)( PDr[r[]^(-1)*V[] PDr[r[] J[]]] - 1/r[] PDr[r[]^2 Eth[Uq[]]] *)
(*			+ 2/r[] Exp[be[]] Eth@Eth@Exp[be[]] - J[]PDr[V[]/r[]] *)
(*			+ NJs[1]+ NJs[2]+ NJs[3]+ NJs[4]+ NJs[5]+ NJs[6]+ NJs[7]*)
(*			+ J[]/r[] (Ps[1] + Ps[2] + Ps[3] + Ps[4])))*)


(* ::Subsubsection::Closed:: *)
(*Verification of PittNull forms*)


(* ::Input:: *)
(*CompareEquations[SWHypersurfaceBeta, SWHypersurfaceBetaPitt]*)


(* ::Input:: *)
(*CompareEquations[SWHypersurfaceQ, SWHypersurfaceQPitt]*)


(* ::Input:: *)
(*CompareEquations[SWHypersurfaceU, SWHypersurfaceUPitt]*)


(* ::Input:: *)
(*CompareEquations[SWHypersurfaceW, SWHypersurfaceWPitt]*)


(* ::Input:: *)
(*CompareEquations[SWEvolution, SWEvolutionPitt]\*)
(*/.{PDr[V[]]:>SWHypersurfaceW[[2]],PDr[be[]]:>SWHypersurfaceBeta[[2]]}//Simplify*)


(* ::Section:: *)
(*Newman-Penrose from Bondi-Sachs quantities*)


(* ::Subsection:: *)
(*Newman-Penrose construction*)


(* ::Subsubsection::Closed:: *)
(*Newman-Penrose definitions*)


(* ::Text:: *)
(*NP vectors*)


(* ::Input:: *)
(*DefTensor[l[a],{M4}];*)
(*DefTensor[m[a],{M4}];*)
(*DefTensor[mb[a],{M4},PrintAs->"\!\(\*OverscriptBox[\(m\), \(_\)]\)"];*)
(*DefTensor[n[a],{M4}];*)


(* ::Text:: *)
(*NP spin coefficients*)


(* ::Input:: *)
(*DefTensor[NPa[],{M4},PrintAs->"\[Alpha]"];DefTensor[NPb[],{M4},PrintAs->"\!\(\*SubscriptBox[\(\[Beta]\), \(NP\)]\)"];DefTensor[NPg[],{M4},PrintAs->"\[Gamma]"];DefTensor[NPe[],{M4},PrintAs->"\[Epsilon]"];*)
(*DefTensor[NPk[],{M4},PrintAs->"\[Kappa]"];DefTensor[NPt[],{M4},PrintAs->"\[Tau]"];DefTensor[NPs[],{M4},PrintAs->"\[Sigma]"];DefTensor[NPr[],{M4},PrintAs->"\[Rho]"];*)
(*DefTensor[NPp[],{M4},PrintAs->"\[Pi]"];DefTensor[NPn[],{M4},PrintAs->"\[Nu]"];DefTensor[NPm[],{M4},PrintAs->"\[Mu]"];DefTensor[NPl[],{M4},PrintAs->"\[Lambda]"];*)


(* ::Subsubsection::Closed:: *)
(*Newman-Penrose substitution rules*)


(* ::Input:: *)
(*SubNPVectors={*)
(*m[a_]:>(-1/Sqrt[2])Module[{A},(1/r[])(basv[a,-A]q[A]*Sqrt[(1+K[])/2] *)
(*									- basv[a,-A]qb[A]J[]*Sqrt[1/(2 * (1+K[]))])],*)
(*mb[a_]:>(-1/Sqrt[2])Conj[Module[{A}, (1/r[])(basv[a,-A]q[A]*Sqrt[(1+K[])/2] *)
(*								- basv[a,-A]qb[A]J[]*Sqrt[1/(2 * (1+K[]))])]],*)
(*n[a_]:>Sqrt[2]Exp[-2be[]]Module[{A},uv[a] - V[]/(2r[]) rv[a]*)
(*							 + (Uqb[]/2) basv[a,-A]q[A] *)
(*								+ Uq[]/2 basv[a,-A]qb[A]],*)
(*l[a_]:>(1/Sqrt[2])rv[a]};*)


(* ::Input:: *)
(*(*SubNPVectors={*)
(*m[a_]\[RuleDelayed](1/Sqrt[2])Module[{A},(1/r[])(basv[a,-A]q[A]*Sqrt[(1+K[])/2] *)
(*									- basv[a,-A]qb[A]J[]*Sqrt[1/(2 * (1+K[]))])],*)
(*mb[a_]\[RuleDelayed](1/Sqrt[2])Conj[Module[{A}, (1/r[])(basv[a,-A]q[A]*Sqrt[(1+K[])/2] *)
(*								- basv[a,-A]qb[A]J[]*Sqrt[1/(2 * (1+K[]))])]],*)
(*n[a_]\[RuleDelayed]Module[{A},uv[a] - V[]/(2r[]) rv[a]*)
(*							 + (Uqb[]/2) basv[a,-A]q[A] *)
(*								+ Uq[]/2 basv[a,-A]qb[A]],*)
(*l[a_]\[RuleDelayed]Exp[-2be[]]rv[a]};*)*)


(* ::Input:: *)
(*(mb[a]/.SubNPVectors)/.{K[]->1,J[]->0,Jb[]->0}*)


(* ::Input:: *)
(*ExpandNPSpinCoeffs=\*)
(*(MakeRule[#,ContractMetrics->None][[1]]&/@*)
(*{{NPk[],-met[-a,-c]m[a]l[b]CD[-b][l[c]]}, {NPt[],-met[-a,-c]m[a]n[b]CD[-b][l[c]]},*)
(* {NPs[],-met[-a,-c]m[a]m[b]CD[-b][l[c]]}, {NPr[],-met[-a,-c]m[a]mb[b]CD[-b][l[c]]},*)
(* {NPp[],met[-a,-c]mb[a]l[b]CD[-b][n[c]]}, {NPn[],met[-a,-c]mb[a]n[b]CD[-b][n[c]]},*)
(* {NPm[],met[-a,-c]mb[a]m[b]CD[-b][n[c]]}, {NPl[],met[-a,-c]mb[a]mb[b]CD[-b][n[c]]},*)
(* {NPe[],(-1/2)met[-a,-c](n[a]l[b]CD[-b][l[c]] - mb[a]l[b]CD[-b][m[c]])},*)
(* {NPg[],(-1/2)met[-a,-c](n[a]n[b]CD[-b][l[c]] - mb[a]n[b]CD[-b][m[c]])},*)
(* {NPb[],(-1/2)met[-a,-c](n[a]m[b]CD[-b][l[c]] - mb[a]m[b]CD[-b][m[c]])},*)
(* {NPa[],(-1/2)met[-a,-c](n[a]mb[b]CD[-b][l[c]] - mb[a]mb[b]CD[-b][m[c]])}});*)


(* ::Subsubsection::Closed:: *)
(*Newman-Penrose simplification utilities*)


(* ::Input:: *)
(*NPExpand[exp_]:=(exp/.SubNPVectors/.MetricValues/.met2toqJ/.Utoq/.KtoJ)//ExpandAll//FullSimplify//Expand*)


(* ::Input:: *)
(*NewmanPenroseScalarToBondi[expr_]:=(((expr/.ExpandNPSpinCoeffs/.SubNPVectors//ChangeCovD//ChristoffelToGradMetric//SimplifyEFEComponent//TensorExpressionToSimplifiedSWScalar))//ExpandAll)/.KtoJ//TensorExpressionToSimplifiedSWScalar//TensorExpressionToSimplifiedSWScalar//FullSimplify;*)


(* ::Subsubsection::Closed:: *)
(*Tests of Newman-Penrose identities*)


(* ::Input:: *)
(*Outer[(#1[a]#2[b]met[-a,-b]//NPExpand)&,{l,n,m,mb},{l,n,m,mb}]//FullSimplify*)


(* ::Subsubsection::Closed:: *)
(*Newman-Penrose automatic rules*)


(* ::Text:: *)
(*Optimization for contracting Newman-Penrose vectors*)


(* ::Input:: *)
(*l/:l[a_]m[b_]/;a==ChangeIndex[b]:=0;*)
(*l/:l[a_]mb[b_]/;a==ChangeIndex[b]:=0;*)
(*l/:l[a_]l[b_]/;a==ChangeIndex[b]:=0;*)
(*l/:l[a_]n[b_]/;a==ChangeIndex[b]:=-1;*)
(*n/:n[a_]m[b_]/;a==ChangeIndex[b]:=0;*)
(*n/:n[a_]mb[b_]/;a==ChangeIndex[b]:=0;*)
(*n/:n[a_]n[b_]/;a==ChangeIndex[b]:=0;*)
(*m/:m[a_]m[b_]/;a==ChangeIndex[b]:=0;*)
(*m/:m[a_]mb[b_]/;a==ChangeIndex[b]:=1;*)
(*mb/:mb[a_]mb[b_]/;a==ChangeIndex[b]:=0;*)


(* ::Subsection:: *)
(*Newman-Penrose Spin Coefficients*)


(* ::Subsubsection::Closed:: *)
(*Simplification utilities*)


(* ::Input:: *)
(*NewmanPenroseScalarToBondi[expr_]:=(expr/.ExpandNPSpinCoeffs/.SubNPVectors//ChangeCovD//ChristoffelToGradMetric//SimplifyEFEComponent)/.{PD[a_][K[]]:>PD[a][K[]/.KtoJ]}//TensorExpressionToSimplifiedSWScalar//SimplifyOnePlusKFactor//SimplifyWithTermSort[GatherScalarDerivFactors]//ToCanonical;*)


(* ::Subsubsection::Closed:: *)
(*Substitution rule for Newman-Penrose spin coefficients*)


(* ::Input:: *)
(*SubNPScalars=#[]->(#[]//NewmanPenroseScalarToBondi)&/@{NPa,NPb,NPg,NPe,NPk,NPt,NPs,NPr,NPp,NPn,NPm,NPl};*)


(* ::Input:: *)
(*NPScalars={NPa,NPb,NPg,NPe,NPk,NPt,NPs,NPr,NPp,NPn,NPm,NPl};*)


(* ::Subsubsection::Closed:: *)
(*Values of Newman-Penrose Spin Coefficients in terms of Bondi quantities*)


(* ::Text:: *)
(*Use W instead of V*)


(* ::Input:: *)
(*DefTensor[W[],{M4}];*)


(* ::Input:: *)
(*VtoW={V[]->r[]^2 W[]+r[]}*)


(* ::Input:: *)
(*Conj[(Sqrt[1+K[]] qb[-A] qb[B] sphd[-B][q[A]])/(8 r[])]//Simplify*)


(* ::Input:: *)
(*q[A]qb[B]sphd[-B][qb[-A]]*)


(* ::Input:: *)
(*Print[#[]==(#[]/.SubNPScalars//ScreenDollarIndices)]&/@NPScalars;*)


(* ::Input:: *)
(*NPl[]/.SubNPScalars//SimplifyWithTermSort[GatherScalarDerivFactors]*)


(* ::Input:: *)
(*NPg[]/.SubNPScalars//SimplifyWithTermSort[GatherScalarDerivFactors]*)


(* ::Input:: *)
(*(E^(-2 be[]) Ethbar[Uqb[]] J[])/(4 Sqrt[2])-(E^(-2 be[]) Eth[Uq[]] Jb[])/(4 Sqrt[2])-(E^(-2 be[]) Eth[Uqb[]] K[])/(4 Sqrt[2])+(E^(-2 be[]) Ethbar[Uq[]] K[])/(4 Sqrt[2])-(E^(-2 be[]) Jb[] PDu[J[]])/(4 Sqrt[2] (1+K[]))+(E^(-2 be[]) J[] PDu[Jb[]])/(Sqrt[2] (4+4 K[]))+(E^(-2 be[]) PDr[V[]])/(2 Sqrt[2] r[])-(E^(-2 be[]) Ethbar[J[]] Jb[] Uq[])/(8 Sqrt[2] (1+K[]))+(E^(-2 be[]) Ethbar[Jb[]] J[] Uq[])/(Sqrt[2] (8+8 K[]))-(E^(-2 be[]) Eth[J[]] Jb[] Uqb[])/(8 Sqrt[2] (1+K[]))+(E^(-2 be[]) Eth[Jb[]] J[] Uqb[])/(Sqrt[2] (8+8 K[]))-(E^(-2 be[]) V[])/(2 Sqrt[2] r[]^2)+(E^(-2 be[]) Jb[] PDr[J[]] V[])/(Sqrt[2] (8 r[]+8 K[] r[]))-(E^(-2 be[]) J[] PDr[Jb[]] V[])/(Sqrt[2] (8 r[]+8 K[] r[]))+(E^(-2 be[]) q[A] qb[-B] Uqb[] sphd[-A][q[B]])/(4 Sqrt[2])-(E^(-2 be[]) q[A] qb[-B] Uqb[] sphd[-A][q[B]])/(8 Sqrt[2] (1+K[]))+(E^(-2 be[]) q[A] qb[-B] Uqb[] sphd[-A][q[B]])/(Sqrt[2] (8+8 K[]))+(E^(-2 be[]) qb[-A] qb[B] Uq[] sphd[-B][q[A]])/(4 Sqrt[2])*)


(* ::Input:: *)
(*(E^(-2 be[]) q[A] qb[-B] Uqb[] sphd[-A][q[B]])/(4 Sqrt[2])-(E^(-2 be[]) q[A] qb[-B] Uqb[] sphd[-A][q[B]])/(8 Sqrt[2] (1+K[]))+(E^(-2 be[]) q[A] qb[-B] Uqb[] sphd[-A][q[B]])/(Sqrt[2] (8+8 K[]))+(E^(-2 be[]) qb[-A] qb[B] Uq[] sphd[-B][q[A]])/(4 Sqrt[2])//ToCanonical//Simplify//ToCanonical*)


(* ::Input:: *)
(*(E^(-2 be[]) q[A] qb[-B] Uqb[] sphd[-A][q[B]])/(4 Sqrt[2])+(E^(-2 be[]) qb[-A] qb[B] Uq[] sphd[-B][q[A]])/(4 Sqrt[2])*)


(* ::Input:: *)
(*NPb[]/.SubNPScalars//SimplifyWithTermSort[GatherScalarDerivFactors]*)


(* ::Input:: *)
(*NPa[]/.SubNPScalars//SimplifyWithTermSort[GatherScalarDerivFactors]*)


(* ::Input:: *)
(*(E^(-2 be[]) Eth[V[]] Jb[] Sqrt[1/(1+K[])])/(2 r[]^2)-(E^(-2 be[]) Ethbar[V[]] Sqrt[1+K[]])/(2 r[]^2)/.VtoW//.ScalarDerivLeibniz*)


(* ::Subsection:: *)
(*Newman-Penrose Weyl scalars*)


(* ::Subsubsection::Closed:: *)
(*Definitions of Weyl scalars*)


(* ::Text:: *)
(*Below, we assume a vacuum solution. This can be relaxed to derive more generic relations, but as all of the CCE Bondi formalism assumes vacuum, there doesn't seem to be a compelling reason to relax the assumption here.*)


(* ::Input:: *)
(*DefTensor[Psi0[],{M4,M2},PrintAs->"\!\(\*SubscriptBox[\(\[CapitalPsi]\), \(0\)]\)"];*)
(*DefTensor[Psi1[],{M4,M2},PrintAs->"\!\(\*SubscriptBox[\(\[CapitalPsi]\), \(1\)]\)"];*)
(*DefTensor[Psi2[],{M4,M2},PrintAs->"\!\(\*SubscriptBox[\(\[CapitalPsi]\), \(2\)]\)"];*)
(*DefTensor[Psi3[],{M4,M2},PrintAs->"\!\(\*SubscriptBox[\(\[CapitalPsi]\), \(3\)]\)"];*)
(*DefTensor[Psi4[],{M4,M2},PrintAs->"\!\(\*SubscriptBox[\(\[CapitalPsi]\), \(4\)]\)"];*)


(* ::Text:: *)
(*Definitions of the 'modified NP spin coefficients' - these will take identical values to the originals, except with the dyad spin coefficient removed.*)
(*These should only be used in the Weyl scalar evaluation, and their use is justified by the below demonstration that the Weyl scalar is independent of the dyad spin coefficient.*)


(* ::Input:: *)
(*DefTensor[mNPa[],{M4},PrintAs->"\[Alpha]'"];DefTensor[mNPb[],{M4},PrintAs->"\[Beta]'"];DefTensor[mNPg[],{M4},PrintAs->"\[Gamma]'"];DefTensor[mNPe[],{M4},PrintAs->"\[Epsilon]'"];*)
(*DefTensor[mNPk[],{M4},PrintAs->"\[Kappa]'"];DefTensor[mNPt[],{M4},PrintAs->"\[Tau]'"];DefTensor[mNPs[],{M4},PrintAs->"\[Sigma]'"];DefTensor[mNPr[],{M4},PrintAs->"\[Rho]'"];*)
(*DefTensor[mNPp[],{M4},PrintAs->"\[Pi]'"];DefTensor[mNPn[],{M4},PrintAs->"\[Nu]'"];DefTensor[mNPm[],{M4},PrintAs->"\[Mu]'"];DefTensor[mNPl[],{M4},PrintAs->"\[Lambda]'"];*)


(* ::Subsubsection::Closed:: *)
(*Weyl scalar substitution rules*)


(* ::Text:: *)
(*There might be something subtly wrong either with this form for Psi3, or with the some of the*)
(*asymptotic dependence inferred from regularity preservation*)


(* ::Input:: *)
(*(*WeylScalarToSpinCoefficients={*)
(*Psi0[]->l[a]CD[-a][NPs[]]-m[a]CD[-a][NPk[]]*)
(*-(NPr[]+Conj[NPr[]])*NPs[]-(3NPe[]-Conj[NPe[]])NPs[]*)
(*+(NPt[]-Conj[NPp[]]+Conj[NPa[]]+3NPb[])NPk[],*)
(*Psi1[]\[Rule]l[a]CD[-a][NPt[]]-n[a]CD[-a][NPk[]]*)
(*-(NPt[]+Conj[NPp[]])NPr[]-(Conj[NPt[]]+NPp[])NPs[]*)
(*-(NPe[]-Conj[NPe[]])NPt[]+(3NPg[]+Conj[NPg[]])NPk[],*)
(*Psi2[]\[Rule]l[a]CD[-a][NPm[]]-m[a]CD[-a][NPp[]]*)
(*-Conj[NPr[]]NPm[]- NPs[]NPl[]-NPp[]Conj[NPp[]]*)
(*+(NPe[]+Conj[NPe[]])NPm[]+(Conj[NPa[]]-NPb[])NPp[]*)
(*+NPk[]NPn[],*)
(*Psi3[]\[Rule]l[a]CD[-a][NPn[]]-n[a]CD[-a][NPp[]]*)
(*-(NPp[]+Conj[NPt[]])NPm[]-(Conj[NPp[]]+NPt[])NPl[]*)
(*-(NPg[]-Conj[NPg[]])NPp[]+(3NPe[]+Conj[NPe[]])NPn[],*)
(*Psi4[]\[Rule]-n[a]CD[-a][NPl[]]+mb[a]CD[-a][NPn[]]*)
(*-(NPm[]+Conj[NPm[]])NPl[]-(3NPg[]-Conj[NPg[]])NPl[]*)
(*+(3NPa[]+Conj[NPb[]]+NPp[]-Conj[NPt[]])NPn[]};*)*)


(* ::Input:: *)
(*WeylScalarToSpinCoefficients={*)
(*Psi0[]->l[a]CD[-a][NPs[]]-m[a]CD[-a][NPk[]]*)
(*-(NPr[]+Conj[NPr[]])*NPs[]-(3NPe[]-Conj[NPe[]])NPs[]*)
(*+(NPt[]-Conj[NPp[]]+Conj[NPa[]]+3NPb[])NPk[],*)
(*Psi1[]->l[a]CD[-a][NPt[]]-n[a]CD[-a][NPk[]]*)
(*-(NPt[]+Conj[NPp[]])NPr[]-(Conj[NPt[]]+NPp[])NPs[]*)
(*-(NPe[]-Conj[NPe[]])NPt[]+(3NPg[]+Conj[NPg[]])NPk[],*)
(*Psi2[]->l[a]CD[-a][NPm[]]-m[a]CD[-a][NPp[]]*)
(*-Conj[NPr[]]NPm[]- NPs[]NPl[]-NPp[]Conj[NPp[]]*)
(*+(NPe[]+Conj[NPe[]])NPm[]+(Conj[NPa[]]-NPb[])NPp[]*)
(*+NPk[]NPn[],*)
(*Psi3[]->mb[a]CD[-a][NPg[]]-n[a]CD[-a][NPa[]]*)
(*+(NPr[]+NPe[])NPn[]-(NPt[]+NPb[])NPl[]*)
(*+(Conj[NPg[]]-Conj[NPm[]])NPa[]*)
(*+(Conj[NPb[]]-Conj[NPt[]])NPg[],*)
(*Psi4[]->-n[a]CD[-a][NPl[]]+mb[a]CD[-a][NPn[]]*)
(*-(NPm[]+Conj[NPm[]])NPl[]-(3NPg[]-Conj[NPg[]])NPl[]*)
(*+(3NPa[]+Conj[NPb[]]+NPp[]-Conj[NPt[]])NPn[]};*)


(* ::Input:: *)
(*WeylScalarToSpinCoefficients={*)
(*Psi0[]->l[a]CD[-a][NPs[]]-m[a]CD[-a][NPk[]]*)
(*-(NPr[]+Conj[NPr[]])*NPs[]-(3NPe[]-Conj[NPe[]])NPs[]*)
(*+(NPt[]-Conj[NPp[]]+Conj[NPa[]]+3NPb[])NPk[],*)
(*Psi1[]->l[a]CD[-a][NPt[]]-n[a]CD[-a][NPk[]]*)
(*-(NPt[]+Conj[NPp[]])NPr[]-(Conj[NPt[]]+NPp[])NPs[]*)
(*-(NPe[]-Conj[NPe[]])NPt[]+(3NPg[]+Conj[NPg[]])NPk[],*)
(*Psi2[]->l[a]CD[-a][NPm[]]-m[a]CD[-a][NPp[]]*)
(*-Conj[NPr[]]NPm[]- NPs[]NPl[]-NPp[]Conj[NPp[]]*)
(*+(NPe[]+Conj[NPe[]])NPm[]+(Conj[NPa[]]-NPb[])NPp[]*)
(*+NPk[]NPn[],*)
(*Psi3[]->l[a]CD[-a][NPn[]]-n[a]CD[-a][NPp[]]*)
(*-(NPp[]+Conj[NPt[]])NPm[]-(Conj[NPp[]]+NPt[])NPl[]*)
(*-(NPg[]-Conj[NPg[]])NPp[]-(3NPe[]+Conj[NPe[]])NPn[],*)
(*Psi4[]->-n[a]CD[-a][NPl[]]+mb[a]CD[-a][NPn[]]*)
(*-(NPm[]+Conj[NPm[]])NPl[]-(3NPg[]-Conj[NPg[]])NPl[]*)
(*+(3NPa[]+Conj[NPb[]]+NPp[]-Conj[NPt[]])NPn[]};*)


(* ::Input:: *)
(*NPtomNP={NPa[]->mNPa[],NPb[]->mNPb[],NPg[]->mNPg[],NPe[]->mNPe[],*)
(*		NPk[]->mNPk[],NPt[]->mNPt[],NPs[]->mNPs[],NPr[]->mNPr[],*)
(*		NPp[]->mNPp[],NPn[]->mNPn[],NPm[]->mNPm[],NPl[]->mNPl[]};*)


(* ::Subsubsection::Closed:: *)
(*Simplification utilities for Weyl scalar expressions*)


(* ::Input:: *)
(*SubMet2ContractedWithq={*)
(*(met2[-A_,-B_]|met2[-B_,-A_])q[A_]:>K[]q[-B]+J[]qb[-B],*)
(*(met2[-A_,-B_]|met2[-B_,-A_])qb[A_]:>Jb[]q[-B]+K[]qb[-B],*)
(*rv[a_]PD[-a_][(met2[-A_,-B_]|met2[-B_,-A_])]q[A_]:>PDr[K[]]q[-B]+PDr[J[]]qb[-B],*)
(*rv[a_]PD[-a_][(met2[-A_,-B_]|met2[-B_,-A_])]qb[A_]:>PDr[Jb[]]q[-B]+PDr[K[]]qb[-B],*)
(*uv[a_]PD[-a_][(met2[-A_,-B_]|met2[-B_,-A_])]q[A_]:>PDu[K[]]q[-B]+PDu[J[]]qb[-B],*)
(*uv[a_]PD[-a_][(met2[-A_,-B_]|met2[-B_,-A_])]qb[A_]:>PDu[Jb[]]q[-B]+PDu[K[]]qb[-B],*)
(*q[-A_]U[A_]:>Uq[],*)
(*qb[-A_]U[A_]:>Uqb[]};*)


(* ::Input:: *)
(*WeylScalarToBondi[exp_]:=(( *)
(*(((exp/.WeylScalarToSpinCoefficients//ChangeCovD)/.SubNPVectors//ExpandAll//ConvertToAngularSphd)/.SubNPScalars//ExpandAll)/.sphdToEth//ExpandAll)/.SubMet2ContractedWithq/.PDsToScalarDerivatives//.ScalarDerivLeibniz//ExpandAll//ToCanonical[#,UseMetricOnVBundle->None]&//ToCanonical)/.Utoq/.Qtoq/.{PDu[K[]]:>PDu[K[]/.KtoJ],PDr[K[]]:>PDr[K[]/.KtoJ]}//.ScalarDerivLeibniz//ExpandAll//ToCanonical//SimplifyWithTermSort[GatherScalarDerivFactors,(#//FullSimplify//SimplifyOnePlusKFactor)&]//SimplifyOnePlusKFactor;*)


(* ::Subsubsection::Closed:: *)
(*Proof of independence of Weyl scalars on the dyad spin coefficients*)


(* ::Text:: *)
(*Here we justify the method of simplification in which we replace the covariant angular derivatives with the spin-weighted derivatives, while dropping the dyad connections from all expressions. This is justified if and only if we can demonstrate that the Weyl scalars are completely independent of the dyad connections.*)
(*Of course, we expect this to be true - the dyad connections are coordinate dependent and the Weyl scalars should be gauge invariants. Nonetheless, this is an important verification step.*)


(* ::Input:: *)
(*VerifyWeylScalarIndependentOfSpinConnection[exp_]:=( *)
(*((((((exp/.WeylScalarToSpinCoefficients//ChangeCovD)*)
(*/.SubNPVectors//ExpandAll//ConvertToAngularSphd)*)
(*/.SubNPScalars//ExpandAll//RemoveIndependentOf[sphd])*)
(*/.sphdToEth//ExpandAll)/.SubMet2ContractedWithq*)
(*/.PDsToScalarDerivatives//.ScalarDerivLeibniz*)
(*//ExpandAll//RemoveIndependentOf[sphd]*)
(*//ToCanonical[#,UseMetricOnVBundle->None]&)//ToCanonical)/.Utoq/.Qtoq/.{PDu[K[]]:>PDu[K[]/.KtoJ],PDr[K[]]:>PDr[K[]/.KtoJ]}//.ScalarDerivLeibniz//ToCanonical//Simplify)==0;*)


(* ::Input:: *)
(*VerifyWeylScalarIndependentOfSpinConnection[Psi0[]]*)


(* ::Input:: *)
(*VerifyWeylScalarIndependentOfSpinConnection[Psi1[]]*)


(* ::Input:: *)
(*VerifyWeylScalarIndependentOfSpinConnection[Psi2[]]*)


(* ::Input:: *)
(*VerifyWeylScalarIndependentOfSpinConnection[Psi3[]]*)


(* ::Input:: *)
(*VerifyWeylScalarIndependentOfSpinConnection[Psi4[]]*)


(* ::Subsubsection::Closed:: *)
(*Weyl scalar expressions*)


(* ::Input:: *)
(*WeylScalarToBondi[Psi0[]]//SimplifyWithTermSort[GatherScalarDerivFactors]*)


(* ::Input:: *)
(*WeylScalarToBondi[Psi1[]]//SimplifyWithTermSort[GatherScalarDerivFactors]*)


(* ::Input:: *)
(**)


(* ::Text:: *)
(*Some of the Weyl scalars have worthwhile condensed forms in full generality in terms of Bondi metric components, while the rest are best expressed as spin-weighted derivatives of versions of the spin coefficients. The below expressions convert the angular derivatives directly to spin-weighted derivatives, which is justified by the above verification that the Weyl scalar is independent of the dyad spin coefficients*)


(* ::Input:: *)
(*ForceSphdToEth[exp_]:=exp/.{*)
(*q[A_]sphd[-A_][ex_]/;IndicesOf[Free][ex]==IndexList[]:>Eth[ex],*)
(*qb[A_]sphd[-A_][ex_]/;IndicesOf[Free][ex]==IndexList[]:>Ethbar[ex]};*)


(* ::Input:: *)
(*WeylScalarToModifiedSpinCoeffs[exp_]:=(exp/.WeylScalarToSpinCoefficients/.SubNPVectors//Expand//ChangeCovD//ConvertToAngularSphd//ExpandAll)/.NPtomNP/.PDsToScalarDerivatives//ForceSphdToEth;*)


(* ::Text:: *)
(*The full set of scalars to be used in the subsequent expressions*)


(* ::Text:: *)
(*Evaluate the below only if you'd really like these expanded forms. They're quite long and not terribly informative, and rather slow to evaluate.*)


(* ::Input:: *)
(*(*WeylScalarToBondi[Psi2[]]//SimplifyWithTermSort[GatherScalarDerivFactors]*)*)


(* ::Input:: *)
(*SWHypersurfaceWCondensed*)


(* ::Input:: *)
(*(*WeylScalarToBondi[Psi3[]]//SimplifyWithTermSort[GatherScalarDerivFactors]*)*)


(* ::Input:: *)
(*(*WeylScalarToBondi[Psi4[]]//SimplifyWithTermSort[GatherScalarDerivFactors]*)*)


(* ::Input:: *)
(*SubmNPScalars=SubNPScalars/.NPtomNP/.{sphd[_][(q|qb)[_]]:>0};*)


(* ::Input:: *)
(*Psi2[]//WeylScalarToModifiedSpinCoeffs*)


(* ::Input:: *)
(*Psi3[]//WeylScalarToModifiedSpinCoeffs*)


(* ::Input:: *)
(*Psi4[]//WeylScalarToModifiedSpinCoeffs*)


(* ::Subsection:: *)
(*Asymptotic dependence in the 'Incompletely flat' gauge*)


(* ::Subsubsection::Closed:: *)
(*Definitions for Asymptotic expansion*)


(* ::Input:: *)
(*DefTensor[Jp[LI[o]],{M4},PrintAs->"J"];DefTensor[Jbp[LI[o]],{M4},PrintAs->"\!\(\*OverscriptBox[\(J\), \(_\)]\)"];*)
(*DefTensor[Uqp[LI[o]],{M4},PrintAs->"U"];DefTensor[Uqbp[LI[o]],{M4},PrintAs->"\!\(\*OverscriptBox[\(U\), \(_\)]\)"];*)
(*DefTensor[Qqp[LI[o]],{M4},PrintAs->"Q"];DefTensor[Qqbp[LI[o]],{M4},PrintAs->"\!\(\*OverscriptBox[\(Q\), \(_\)]\)"];*)
(*DefTensor[bep[LI[o]],{M4},PrintAs->"\[Beta]"];DefTensor[Wp[LI[o]],{M4},PrintAs->"W"];*)


(* ::Input:: *)
(*DefTensor[Kp[LI[o]],{M4},PrintAs->"K"];*)


(* ::Text:: *)
(*Ord is an order marker in these expansions to indicate where the expansion has terminated so that we never trust a series beyond the point we've expanded it (and so that we don't have to deal with the comparatively heavy Mathematica built-in series output object)*)


(* ::Input:: *)
(*DefTensor[invr[],{M4,M2},PrintAs->"\!\(\*SuperscriptBox[\(r\), \(-1\)]\)"];*)


(* ::Input:: *)
(*DefTensor[OrdSym[],{M4,M2},PrintAs->"O"];*)


(* ::Input:: *)
(*Eth[OrdSym[LI[exp_]]]:=OrdSym[LI[exp]];*)
(*PDr[OrdSym[LI[exp_]]]:=0;*)
(*PDu[OrdSym[LI[exp_]]]:=OrdSym[LI[exp]];*)


(* ::Input:: *)
(*Unprotect[Times]*)
(*OrdSym/:OrdSym[LI[num_]]+a_/;FreeQ[a,r|invr]:=OrdSym[LI[num]];*)
(*Times/:OrdSym[LI[num1_]]*r[]^n_ + OrdSym[LI[num2_]]*r[]^m_:=OrdSym[LI[num1]]*r[]^(Max[n,m])*)
(*OrdSym/:a_?FreeQ[#,r|invr]&*OrdSym[LI[num_]]:=OrdSym[LI[num]];*)
(*Protect[Times]*)


(* ::Text:: *)
(*Unique trick used so that these things don't inadvertently cancel*)


(* ::Input:: *)
(*Ord:=OrdSym[LI[Unique[]]];*)


(* ::Subsubsection::Closed:: *)
(*Incompletely flat expansion*)


(* ::Text:: *)
(*Note that several of the coefficients are intentionally omitted because they vanish in the Incompletely flat gauge*)


(* ::Input:: *)
(*ExpandIFBondiQuantitiesNearScri={*)
(*J[]->Jp[LI[1]]/r[] + Jp[LI[3]]/r[]^3+Jp[LI[4]]/r[]^4+Ord/r[]^5,*)
(*Jb[]->Jbp[LI[1]]/r[] + Jbp[LI[3]]/r[]^3+ Jbp[LI[4]]/r[]^4+Ord/r[]^5,*)
(*Uq[]->Uqp[LI[1]]/r[] + Uqp[LI[2]]/r[]^2+ Uqp[LI[3]]/r[]^3+Ord/r[]^4,*)
(*Uqb[]->Uqbp[LI[1]]/r[] + Uqbp[LI[2]]/r[]^2+ Uqbp[LI[3]]/r[]^3+Ord/r[]^4,*)
(*Qq[]->-2Eth[bep[LI[0]]] + Qqp[LI[1]]/r[] + Qqp[LI[2]]/r[]^2+ Qqp[LI[3]]/r[]^3+Ord/r[]^4,*)
(*Qqb[]->-2Ethbar[bep[LI[0]]] + Qqbp[LI[1]]/r[] + Qqbp[LI[2]]/r[]^2+ Qqbp[LI[3]]/r[]^3+Ord/r[]^4,*)
(*be[]->bep[LI[0]]  + bep[LI[2]]/r[]^2+ bep[LI[3]]/r[]^3+Ord/r[]^4,*)
(*V[]->r[] + r[]^2 (Wp[LI[1]]/r[]+Wp[LI[2]]/r[]^2 +Wp[LI[3]]/r[]^3 )+Ord/r[]^2,*)
(*K[]->1+1/2Jp[LI[1]]Jbp[LI[1]]/r[]^2 + Ord/r[]^4};*)


(* ::Input:: *)
(*WeylScalarToBondi[Psi0[]]*)


(* ::Input:: *)
(*Series[Sqrt[1+ep^2 J Jb],{ep,0,4}]*)


(* ::Subsubsection::Closed:: *)
(*Incompletely flat expansion automatic rules*)


(* ::Text:: *)
(*Note that we've performed an expansion in r, so the coefficients of that expansion are necessarily independent of r.*)


(* ::Input:: *)
(*PDr[Jp[LI[o_]]]:=0;*)
(*PDr[Jbp[LI[o_]]]:=0;*)
(*PDr[PDu[Jp[LI[o_]]]]:=0;*)
(*PDr[PDu[Jbp[LI[o_]]]]:=0;*)
(*PDr[Qqp[LI[o_]]]:=0;*)
(*PDr[Qqbp[LI[o_]]]:=0;*)
(*PDr[Uqp[LI[o_]]]:=0;*)
(*PDr[Uqbp[LI[o_]]]:=0;*)
(*PDr[bep[LI[o_]]]:=0;*)
(*PDr[Wp[LI[o_]]]:=0;*)


(* ::Input:: *)
(*Conj[bep[LI[o_]]]:=bep[LI[o]];*)
(*Conj[Wp[LI[o_]]]:=Wp[LI[o]];*)


(* ::Input:: *)
(*Conj[Jp[LI[o_]]]:=Jbp[LI[o]];*)
(*Conj[Jbp[LI[o_]]]:=Jp[LI[o]];*)


(* ::Input:: *)
(*WeylScalarAsymptoticSeriesFromSpinCoeffs[NPs[],2]*)


(* ::Subsubsection::Closed:: *)
(*Incompletely flat simplification utilities*)


(* ::Input:: *)
(*mNPScalars={mNPa[],mNPb[],mNPg[],mNPe[],*)
(*mNPk[],mNPt[],mNPs[],mNPr[],*)
(*mNPp[],mNPn[],mNPm[],mNPl[]};*)


(* ::Input:: *)
(*MakemNPSeries[exp_]:=((Series[(exp/.SubmNPScalars/.ExpandIFBondiQuantitiesNearScri)//.ScalarDerivLeibniz/.{r[]->1/invr[]},{invr[],0,4}]//Normal)/.{invr[]->1/r[]})+Ord/r[]^5*)


(* ::Input:: *)
(*mNPLeading=#->(#//MakemNPSeries)&/@mNPScalars;*)


(* ::Input:: *)
(*WeylScalarAsymptoticSeriesFromSpinCoeffs[exp_,order_]:=((Series[((exp//WeylScalarToModifiedSpinCoeffs)/.ExpandIFBondiQuantitiesNearScri/.mNPLeading//.ScalarDerivLeibniz//ExpandAll)/.{r[]->1/invr[]},{invr[],0,order}]//Normal)/.{invr[]->1/r[]})+Ord/r[]^(order+1);*)


(* ::Input:: *)
(*WeylScalarAsymptoticSeriesFromBondi[exp_,order_]:=*)
(*((Series[((exp//WeylScalarToBondi)/.ExpandIFBondiQuantitiesNearScri//ExpandAll)//.ScalarDerivLeibniz/.{r[]->1/invr[]},{invr[],0,order}]//Normal)/.{invr[]->1/r[]}) + Ord/r[]^(order+1);*)


(* ::Subsubsection::Closed:: *)
(*Perturbative expansion of the hypersurface equations*)


(* ::Input:: *)
(*ExpandFullBondiQuantitiesNearScri[4]={*)
(*J[]->Jp[LI[1]]/r[]+Jp[LI[2]]/r[]^2 + Jp[LI[3]]/r[]^3+Ord/r[]^4,*)
(*Jb[]->Jbp[LI[1]]/r[]+Jbp[LI[2]]/r[]^2 + Jbp[LI[3]]/r[]^3+Ord/r[]^4,*)
(*Uq[]->Uqp[LI[1]]/r[] + Uqp[LI[2]]/r[]^2+ Uqp[LI[3]]/r[]^3+Ord/r[]^4,*)
(*Uqb[]->Uqbp[LI[1]]/r[] + Uqbp[LI[2]]/r[]^2+ Uqbp[LI[3]]/r[]^3+Ord/r[]^4,*)
(*Qq[]->Qqp[LI[0]] + Qqp[LI[1]]/r[] + Qqp[LI[2]]/r[]^2+ Qqp[LI[3]]/r[]^3+Ord/r[]^4,*)
(*Qqb[]->Qqbp[LI[0]] + Qqbp[LI[1]]/r[] + Qqbp[LI[2]]/r[]^2+ Qqbp[LI[3]]/r[]^3+Ord/r[]^4,*)
(*be[]->bep[LI[0]] +bep[LI[1]]/r[]  + bep[LI[2]]/r[]^2+ bep[LI[3]]/r[]^3+Ord/r[]^4,*)
(*V[]->r[] + r[]^2 (Wp[LI[1]]/r[]+Wp[LI[2]]/r[]^2 +Wp[LI[3]]/r[]^3 )+Ord/r[]^2};*)
(*ExpandFullBondiQuantitiesNearScri[3]={*)
(*J[]->Jp[LI[1]]/r[]+Jp[LI[2]]/r[]^2 +Ord/r[]^3,*)
(*Jb[]->Jbp[LI[1]]/r[]+Jbp[LI[2]]/r[]^2 +Ord/r[]^3,*)
(*Uq[]->Uqp[LI[1]]/r[] + Uqp[LI[2]]/r[]^2+Ord/r[]^3,*)
(*Uqb[]->Uqbp[LI[1]]/r[] + Uqbp[LI[2]]/r[]^2+Ord/r[]^3,*)
(*Qq[]->Qqp[LI[0]] + Qqp[LI[1]]/r[] + Qqp[LI[2]]/r[]^2+Ord/r[]^3,*)
(*Qqb[]->Qqbp[LI[0]] + Qqbp[LI[1]]/r[] + Qqbp[LI[2]]/r[]^2+Ord/r[]^3,*)
(*be[]->bep[LI[0]] +bep[LI[1]]/r[]  + bep[LI[2]]/r[]^2+Ord/r[]^3,*)
(*V[]->r[] + r[]^2 (Wp[LI[1]]/r[]+Wp[LI[2]]/r[]^2  )+Ord/r[]};*)


(* ::Input:: *)
(*FullAsymptoticSeriesFromBondi[exp_,order_,replacementorder_]:=*)
(*((Series[(exp/.ExpandFullBondiQuantitiesNearScri[replacementorder]//ExpandAll)//.ScalarDerivLeibniz/.{r[]->1/invr[]},{invr[],0,order}]//Normal)/.{invr[]->1/r[]}) + Ord/r[]^(order+1);*)


(* ::Input:: *)
(*BetaSeries=FullAsymptoticSeriesFromBondi[#,3,3]&/@((SWHypersurfaceBetaCondensed/.SubSuplimentaryScalars)//.ScalarDerivLeibniz/.JtoKRule//Expand)*)


(* ::Input:: *)
(*(Coefficient[BetaSeries[[1]],r[]^(-#)]==Coefficient[BetaSeries[[2]],r[]^(-#)])&/@Range[3]*)


(* ::Input:: *)
(*QSeries=(FullAsymptoticSeriesFromBondi[#,1,3]&/@((SWHypersurfaceQCondensed/.SubSuplimentaryScalars)//.ScalarDerivLeibniz/.KtoJ//Expand)*)
(*			/.CumulativeHypersurfaceConditions//.ScalarDerivLeibniz//SimplifyWithTermSort[GatherScalarDerivFactors])*)


(* ::Input:: *)
(*CumulativeHypersurfaceConditions=\*)
(*{bep[LI[1]]:>0,*)
(*bep[LI[2]]:>-(Jp[LI[1]]Jbp[LI[1]])/16,*)
(*Qqp[LI[0]]:>-2Eth@bep[LI[0]],*)
(*Qqp[LI[1]]:>Ethbar@Jp[LI[1]],*)
(*Qqbp[LI[0]]:>-2Ethbar@bep[LI[0]],*)
(*Qqbp[LI[1]]:>Eth@Jbp[LI[1]]};*)


(* ::Input:: *)
(*(FullAsymptoticSeriesFromBondi[#,3,3]&/@SWHypersurfaceUCondensed)/.CumulativeHypersurfaceConditions*)


(* ::Input:: *)
(*CumulativeHypersurfaceConditions=\*)
(*{bep[LI[1]]:>0,*)
(*bep[LI[2]]:>-(Jp[LI[1]]Jbp[LI[1]])/16,*)
(*Qqp[LI[0]]:>-2Eth@bep[LI[0]],*)
(*Qqp[LI[1]]:>Ethbar@Jp[LI[1]],*)
(*Qqbp[LI[0]]:>-2Ethbar@bep[LI[0]],*)
(*Qqbp[LI[1]]:>Eth@Jbp[LI[1]],*)
(*Uqp[LI[1]]:>2Exp[2bep[LI[0]]]Eth@bep[LI[0]],*)
(*Uqp[LI[2]]:>-Exp[2bep[LI[0]]]/2(Ethbar@Jp[LI[1]]+2Ethbar@bep[LI[0]]Jp[LI[1]]),*)
(*Uqbp[LI[1]]:>2Exp[2bep[LI[0]]]Ethbar@bep[LI[0]],*)
(*Uqbp[LI[2]]:>-Exp[2bep[LI[0]]]/2(Eth@Jbp[LI[1]]+2Eth@bep[LI[0]]Jbp[LI[1]])*)
(*};*)


(* ::Input:: *)
(*FullAsymptoticSeriesFromBondi[#,1,3]&/@(SWHypersurfaceWCondensed/.SubSuplimentaryScalars//.ScalarDerivLeibniz/.KtoJ//Expand)/.CumulativeHypersurfaceConditions//.ScalarDerivLeibniz//SimplifyWithTermSort[GatherScalarDerivFactors]//Expand*)


(* ::Input:: *)
(*CumulativeHypersurfaceConditions=\*)
(*{bep[LI[1]]:>0,*)
(*bep[LI[2]]:>-(Jp[LI[1]]Jbp[LI[1]])/16,*)
(*Qqp[LI[0]]:>-2Eth@bep[LI[0]],*)
(*Qqp[LI[1]]:>Ethbar@Jp[LI[1]],*)
(*Qqbp[LI[0]]:>-2Ethbar@bep[LI[0]],*)
(*Qqbp[LI[1]]:>Eth@Jbp[LI[1]],*)
(*Uqp[LI[1]]:>2Exp[2bep[LI[0]]]Eth@bep[LI[0]],*)
(*Uqp[LI[2]]:>-Exp[2bep[LI[0]]]/2(Ethbar@Jp[LI[1]]+2Ethbar@bep[LI[0]]Jp[LI[1]]),*)
(*Uqbp[LI[1]]:>2Exp[2bep[LI[0]]]Ethbar@bep[LI[0]],*)
(*Uqbp[LI[2]]:>-Exp[2bep[LI[0]]]/2(Eth@Jbp[LI[1]]+2Eth@bep[LI[0]]Jbp[LI[1]]),*)
(*Wp[LI[1]]:>Exp[2bep[LI[0]]] -1 +2Exp[2 bep[LI[0]]]Eth@Ethbar@bep[LI[0]]+4Exp[2bep[LI[0]]]Eth@bep[LI[0]]Ethbar@bep[LI[0]]*)
(*};*)


(* ::Input:: *)
(*FullAsymptoticSeriesFromBondi[#,2,3]&/@(SWEvolutionCondensed/.SubSuplimentaryScalars//.ScalarDerivLeibniz/.KtoJ//ReleaseHold//Expand)/.CumulativeHypersurfaceConditions//.ScalarDerivLeibniz//SimplifyWithTermSort[GatherScalarDerivFactors]*)


(* ::Subsubsection::Closed:: *)
(*Weyl scalars in Incompletely flat gauge*)


(* ::Text:: *)
(*Note the verification of Peeling.*)


(* ::Input:: *)
(*WeylScalarAsymptoticSeriesFromSpinCoeffs[Psi4[],1]*)


(* ::Input:: *)
(*WeylScalarAsymptoticSeriesFromSpinCoeffs[Psi4[],1]/.CumulativeHypersurfaceConditions//.ScalarDerivLeibniz//Expand//SimplifyWithTermSort[GatherScalarDerivFactors]*)


(* ::Text:: *)
(*Psi 3 benefits from a bit of extra processing*)


(* ::Input:: *)
(*WeylScalarAsymptoticSeriesFromSpinCoeffs[Psi3[],2]*)


(* ::Input:: *)
(*(WeylScalarAsymptoticSeriesFromSpinCoeffs[Psi3[],2]/.CumulativeHypersurfaceConditions//.ScalarDerivLeibniz//Expand*)
(*//SimplifyWithTermSort[GatherScalarDerivFactors])/.{Eth@Ethbar[bep[LI[0]]]:>Ethbar@Eth[bep[LI[0]]]}//Simplify//Expand*)


(* ::Input:: *)
(*WeylScalarAsymptoticSeriesFromSpinCoeffs[Psi2[],3]*)


(* ::Input:: *)
(*WeylScalarAsymptoticSeriesFromSpinCoeffs[Psi2[],3]/.CumulativeHypersurfaceConditions//.ScalarDerivLeibniz//Expand//SimplifyWithTermSort[GatherScalarDerivFactors]*)


(* ::Input:: *)
(*%410/.{bep[LI[0]]:>0}//.ScalarDerivLeibniz*)


(* ::Input:: *)
(*WeylScalarAsymptoticSeriesFromBondi[Psi1[],4]/.{Qqp[LI[1]]:>Ethbar@Jp[LI[1]]}*)


(* ::Input:: *)
(*%413/.CumulativeHypersurfaceConditions//.ScalarDerivLeibniz//Expand*)


(* ::Input:: *)
(*Conj[Qqbp[LI[n_]]]:=Qqp[LI[n]];*)
(*Conj[Qqp[LI[n_]]]:=Qqbp[LI[n]];*)


(* ::Input:: *)
(*WeylScalarAsymptoticSeriesFromBondi[Psi0[],5]*)


(* ::Input:: *)
(*%416/.CumulativeHypersurfaceConditions//Simplify//Expand*)


(* ::Input:: *)
(*InputForm@%*)


(* ::Input:: *)
(*WeylScalarAsymptoticSeriesFromSpinCoeffs[Psi0[],5]*)


(* ::Input:: *)
(*WeylScalarFullAsymptoticSeriesFromBondi[Psi0[],4]*)


(* ::Input:: *)
(*WeylScalarFullAsymptoticSeriesFromBondi[Psi0[],5]/.{Jp[LI[0]]:>0,Jbp[LI[0]]:>0,Kp[LI[0]]:>1,bep[LI[1]]:>0}//Simplify*)


(* ::Input:: *)
(*SWHypersurfaceBeta*)


(* ::Input:: *)
(*WeylScalarAsymptoticSeriesFromSpinCoeffs[SWHypersurfaceBeta,4]*)


(* ::Input:: *)
(*Psi10=Coefficient[WeylScalarAsymptoticSeriesFromSpinCoeffs[Psi1[],4],r[]^(-4)]*)


(* ::Input:: *)
(*Psi20=Coefficient[WeylScalarAsymptoticSeriesFromSpinCoeffs[Psi2[],3],r[]^(-3)]*)


(* ::Input:: *)
(*Conj[Uqp[LI[n_]]]:=Uqbp[LI[n]]*)
(*Conj[Uqbp[LI[n_]]]:=Uqp[LI[n]]*)


(* ::Input:: *)
(*Psi20-Conj[Psi20]//Expand//Simplify*)


(* ::Section:: *)
(*Characteristic equations in numerical coordinates*)


(* ::Subsubsection::Closed:: *)
(*Definitions for numerical coordinates*)


(* ::Text:: *)
(*We denote the numerical radius 'y'*)


(* ::Input:: *)
(*DefTensor[y[],{M4}];*)


(* ::Input:: *)
(*DefTensor[OneMinusY[],{M4},PrintAs->"(1-y)"];*)


(* ::Text:: *)
(*The Bondi radius of the worldtube*)


(* ::Input:: *)
(*DefTensor[R[],{M4,M2}];*)


(* ::Text:: *)
(*A common combination*)


(* ::Input:: *)
(*DefTensor[nEthRuponR[],{M4,M2},PrintAs->"\!\(\*FractionBox[\(\[PartialD]' R\), \(R\)]\)"];*)
(*DefTensor[nEthbarRuponR[],{M4,M2},PrintAs->"\!\(\*FractionBox[\(\*OverscriptBox[\(\[PartialD]\), \(_\)]' R\), \(R\)]\)"];*)


(* ::Text:: *)
(*Use H instead of \!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(u\)]J\)*)


(* ::Input:: *)
(*DefTensor[H[],{M4}];*)
(*DefTensor[Hb[],{M4},PrintAs->"\!\(\*OverscriptBox[\(H\), \(_\)]\)"];*)


(* ::Text:: *)
(*Scalar derivatives in the numerical coordinates*)


(* ::Input:: *)
(*DefInertHead[PDy,PrintAs->"\!\(\*SubscriptBox[\(\[PartialD]\), \(y\)]\)"];*)
(*DefInertHead[PDnu,PrintAs->"\!\(\*SubscriptBox[\(\[PartialD]\), SuperscriptBox[\(u\), \('\)]]\)"];*)
(*DefInertHead[nEth,PrintAs->"\!\(\*SuperscriptBox[\(\[PartialD]\), \('\)]\)"];*)
(*DefInertHead[nEthbar,PrintAs->"\!\(\*SuperscriptBox[OverscriptBox[\(\[PartialD]\), \(_\)], \('\)]\)"];*)


(* ::Text:: *)
(*The list of scalar derivatives now needs to be expanded*)


(* ::Input:: *)
(*ScalarPDList={PDr,PDu,Eth,Ethbar,PDy,PDnu,nEth,nEthbar};*)


(* ::Subsubsection::Closed:: *)
(*Coordinate transformation replacement rules*)


(* ::Input:: *)
(*ScalarPDToNumericalCoords=*)
(*	{r[]:>2R[]/(1 - y[]),*)
(*	 PDr[exp_]:>((1 - y[])^2/(2R[]))PDy[exp],*)
(*	 PDu[exp_]:>PDnu[exp] - (PDnu[R[]] (1 - y[])/R[])PDy[exp]};*)


(* ::Input:: *)
(*PDuJtoH={PDu[J[]]->H[],PDu[Jb[]]->Hb[]};*)


(* ::Input:: *)
(*nEthRToRatios={nEth[R[]]->R[]nEthRuponR[],nEthbar[R[]]->R[]nEthbarRuponR[]};*)


(* ::Subsubsection::Closed:: *)
(*Numerical coordinates automatic rules*)


(* ::Text:: *)
(*derivative rules:*)


(* ::Input:: *)
(*PDy[R[]]:=0;*)
(*PDy[PDnu[R[]]]:=0;*)
(*PDy[y[]]:=1;*)
(*PDnu[y[]]:=0;*)
(*Eth[y[]]=-nEth[R[]]*(1-y[])/R[];*)
(*Ethbar[y[]]=-nEthbar[R[]]*(1-y[])/R[];*)
(*PDy[OneMinusY[]]:=-1;*)
(*Eth[OneMinusY[]]=nEth[R[]]*(1-y[])/R[];*)
(*Ethbar[OneMinusY[]]=nEthbar[R[]]*(1-y[])/R[];*)
(*Eth[R[]]=nEth[R[]];*)
(*Ethbar[R[]]=nEthbar[R[]];*)


(* ::Input:: *)
(*PDy[nEth[exp_]]:=nEth[PDy[exp]];*)
(*PDy[nEthbar[exp_]]:=nEthbar[PDy[exp]];*)


(* ::Input:: *)
(*nEth[y[]]:=0;*)
(*nEthbar[y[]]:=0;*)


(* ::Input:: *)
(*PDy[Ethbar[exp_]]:=Ethbar@PDy@exp + Ethbar@R[]/R[] PDy@exp;*)
(*PDy[Eth[exp_]]:=Eth@PDy@exp + Eth@R[]/R[] PDy@exp;*)


(* ::Text:: *)
(*Conjugation rules*)


(* ::Input:: *)
(*Conj[y[]]=y[];*)
(*Conj[PDy[exp_]]:=PDy[Conj[exp]];*)
(*Conj[PDnu[exp_]]:=PDnu[Conj[exp]];*)
(*Conj[nEth[exp_]]:=nEthbar[Conj[exp]];*)
(*Conj[R[]]=R[];*)
(*Conj[W[]]=W[];*)
(*Conj[H[]]=Hb[];*)
(*Conj[Hb[]]=H[];*)


(* ::Subsubsection::Closed:: *)
(*L AT EX rendering rules for numerical coordinates*)


(* ::Input:: *)
(*Tex[PDy[exp_]]:="\\partial_y ("<>Tex[exp]<>")";*)


(* ::Input:: *)
(*Tex[nEth[exp_]]:="\\dh^\\prime ("<>Tex[exp]<>")";*)
(*Tex[nEthbar[exp_]]:="\\bar{\\dh}^\\prime ("<>Tex[exp]<>")";*)


(* ::Subsubsection::Closed:: *)
(*Simplification utilities*)


(* ::Input:: *)
(*BondiScalarList={J[],Jb[],be[],Uq[],Uqb[],Qq[],Qqb[],V[]};*)


(* ::Text:: *)
(*A utility for applying the scalar derivative Leibniz rule while preserving the derivative forms which involve the Bondi scalars*)


(* ::Input:: *)
(*PartialScalarDerivLeibniz=*)
(*{der_[a_+b_]/;MemberQ[ScalarPDList,der]:>(der[a + b]/.ScalarDerivLeibniz),*)
(* der_[a_*b_]/;And@@(FreeQ[a,#]&/@BondiScalarList):>b*(der[a]//.ScalarDerivLeibniz) + a*der[b],*)
(* der_[a_]/;And@@(FreeQ[a,#]&/@BondiScalarList):>(der[a]//.ScalarDerivLeibniz)};*)


(* ::Input:: *)
(*CharacteristicEquationToNumericCoords[exp_, factor_]:=FullSimplify/@((exp/.VtoW//.ScalarPDToNumericalCoords//.PartialScalarDerivLeibniz)*factor)*)


(* ::Subsubsection::Closed:: *)
(*Extra scalar definitions for condensed forms*)


(* ::Input:: *)
(*DefTensor[scAQn[],{M4},PrintAs->"A\!\(\*SubscriptBox[\('\), \(Q\)]\)"];*)
(*Tex[scAQn[]]:="\\mathcal{A}_Q^{\\prime}";*)
(*Tex[Conj[scAQn[]]]:="\\bar{\\mathcal{A}}_Q^{\\prime}";*)


(* ::Input:: *)
(*DefTensor[scAWn[],{M4},PrintAs->"\!\(\*SubscriptBox[\(A\), \(W\)]\)"];*)
(*Tex[scAWn[]]:="\\mathcal{A}_W^{\\prime}";*)
(*Tex[Conj[scAWn[]]]:="\\bar{\\mathcal{A}}_W^{\\prime}";*)


(* ::Input:: *)
(*DefTensor[scAJn[],{M4},PrintAs->"A\!\(\*SubscriptBox[\('\), \(J\)]\)"];*)
(*DefTensor[scBJn[],{M4},PrintAs->"B\!\(\*SubscriptBox[\('\), \(J\)]\)"];*)
(*DefTensor[scCJn[],{M4},PrintAs->"C\!\(\*SubscriptBox[\('\), \(J\)]\)"];*)
(*DefTensor[scDJn[],{M4},PrintAs->"D\!\(\*SubscriptBox[\('\), \(J\)]\)"];*)


(* ::Input:: *)
(*Tex[scAJn[]]:="\\mathcal{A}_J^{\\prime}";*)
(*Tex[scBJn[]]:="\\mathcal{B}_J^{\\prime}";*)
(*Tex[scCJn[]]:="\\mathcal{C}_J^{\\prime}";*)
(*Tex[scDJn[]]:="\\mathcal{D}_J^{\\prime}";*)
(*Tex[Conj[scAJn[]]]:="\\bar{\\mathcal{A}}_J^{\\prime}";*)
(*Tex[Conj[scBJn[]]]:="\\bar{\\mathcal{B}}_J^{\\prime}";*)
(*Tex[Conj[scCJn[]]]:="\\bar{\\mathcal{C}}_J^{\\prime}";*)
(*Tex[Conj[scDJn[]]]:="\\bar{\\mathcal{D}}_J^{\\prime}";*)


(* ::Input:: *)
(*SubNumericSuplimentaryScalars={*)
(*scAQn[]->scAQnValue,scAWn[]->scAWnValue,*)
(*scAJn[]->scAJnValue, *)
(*scBJn[LI[0]]->scBJnValue[LI[0]],*)
(*scBJn[LI[1]]->scBJnValue[LI[1]],*)
(*scBJn[LI[2]]->scBJnValue[LI[2]],*)
(*scBJn[LI[3]]->scBJnValue[LI[3]],*)
(*scCJn[]->scCJnValue, scDJn[]->scDJnValue};*)


(* ::Subsubsection::Closed:: *)
(*Characteristic equations in numerical coordinates*)


(* ::Input:: *)
(*SWHypersurfaceBetaNumeric=CharacteristicEquationToNumericCoords[SWHypersurfaceBetaCondensed,(2R[]/(1-y[])^2)]*)


(* ::Input:: *)
(*SWHypersurfaceQNumeric=CharacteristicEquationToNumericCoords[SWHypersurfaceQCondensed,((1-y[])/(2R[]))]*)


(* ::Input:: *)
(*SWHypersurfaceUNumeric=CharacteristicEquationToNumericCoords[SWHypersurfaceUCondensed,2R[]/(1-y[])^2]*)


(* ::Input:: *)
(*SWHypersurfaceWNumeric=Simplify/@((CharacteristicEquationToNumericCoords[SWHypersurfaceWCondensed,1/2]-1/2)*(1-y[])/R[])*)


(* ::Input:: *)
(*SWEvolutionNumeric=CharacteristicEquationToNumericCoords[SWEvolutionCondensed,1]*)


(* ::Subsubsection::Closed:: *)
(*Condensed Characteristic equations in numerical coordinates from manual simplification*)


(* ::Input:: *)
(*SWHypersurfaceBetaNumericCondensed=( *)
(*PDy[be[]]*)
(*==(1 - y[])/8 ( PDy[J[]]PDy[Jb[]] - 1/(4K[]^2) PDy[J[]Jb[]]^2))*)


(* ::Input:: *)
(*SWHypersurfaceQNumericCondensed=( *)
(*(1-y[])PDy[Qq[]] + 2Qq[] *)
(*== -4 Eth@be[] *)
(*	-(1-y[])( - 2 Eth[PDy[be[]]] + Ethbar[PDy[J[]]]/K[] + 2 scAQn[] *)
(*				+ 2J[]/K[] Conj[scAQn[]]  - 2 Eth@R[] PDy@be[]/R[] *)
(*				+ Ethbar@R[] PDy@J[]/(R[]K[])))*)


(* ::Input:: *)
(*scAQnValue=R[]/(1-y[])^2(scAQ[]/.SubSuplimentaryScalars)*)


(* ::Input:: *)
(*SWHypersurfaceUNumericCondensed =SWHypersurfaceUNumeric*)


(* ::Input:: *)
(*SWHypersurfaceWNumericCondensed=( *)
(*(1-y[])PDy[W[]] + 2 W[]*)
(*==Ethbar@Uq[] + Eth@Uqb[]*)
(*+(1 - y[])(Exp[2be[]]/(4R[]) (scAWn[] + Conj[scAWn[]]) + 1/4 Eth@PDy@Uqb[] *)
(*		+ 1/4 Ethbar@PDy@Uq[]+(1/4)Eth@R[] PDy@Uqb[]/R[]*)
(*		+ (1/4) Ethbar@R[] PDy@Uq[]/R[]- 1/(2 R[])))*)


(* ::Input:: *)
(*scAWnValue=scAWValue*)


(* ::Input:: *)
(*SWEvolutionNumericCondensed=*)
(*((1-y[])PDy[H[]] +(1-y[])(scDJn[] J[] H[] + Conj[scDJn[]] J[] Hb[]) + H[]*)
(*	== J[](scBJn[LI[0]] + Conj[scBJn[LI[0]]])*)
(*		-1/2 Eth[J[] Uqb[]] - J[]Eth@Uqb[] - 1/2 J[] Ethbar@Uq[] *)
(*		- K[] Eth@Uq[] - 1/2 Uq[] Ethbar@J[]*)
(*		+(1-y[])**)
(*			(E^(2*be[])/(2R[])**)
(*				(scCJn[]+(Conj[scCJn[]]*J[]^2)/K[]^2-(scAJn[]+Conj[scAJn[]])*J[]*)
(*				+Eth[Eth[be[]]]-Eth[Qq[]]/2*)
(*				+Eth[J[]*(-2*Ethbar[be[]]+Qqb[])]/(4*K[])*)
(*				-(Eth[Qqb[]]*J[])/(4*K[])+(Eth[be[]]-Qq[]/2)^2)*)
(*				+J[](scBJn[LI[1]]+Conj[scBJn[LI[1]]])*)
(*				- 1/2(Eth[PDy@J[] Uqb[]] + Uq[] Ethbar@PDy@J[] + Uq[]Ethbar@R[] PDy@J[]/R[])*)
(*				+PDy@Jb[]((K[]/2 - 1/(2K[])) J[] Eth@Uq[] - 1/4  J[]^2 (Ethbar@Uq[] - Eth@Uqb[]))*)
(*				-PDy@J[](1/(2K[])Jb[]Eth@Uq[] - 1/2 K[] J[] Ethbar@Uqb[]*)
(*						+ 1/4 K[]^2 (Eth@Uqb[] - Ethbar@Uq[]) -1/2 (- Uqb[]nEth[R[]]/R[] + W[])))*)
(*		+(1-y[])^2**)
(*			(J[](scBJn[LI[2]]+Conj[scBJn[LI[2]]])*)
(*			+ 1/2  (- PDy@J[]/R[] + 2/R[] PDnu@R[] PDy@PDy@J[] + W[] PDy@PDy@J[])*)
(*			+PDy@J[](1/2 PDy@W[] + 1/(2R[])))*)
(*		+(1-y[])^3**)
(*			(J[](scBJn[LI[3]]+Conj[scBJn[LI[3]]])+ PDy@PDy@J[]/(4 R[])))*)


(* ::Input:: *)
(*scAJnValue=scAJValue*)


(* ::Input:: *)
(*scBJnValue[LI[0]]=W[]*)


(* ::Input:: *)
(*scBJnValue[LI[1]]=(1/4 Uqb[]  Eth[J[] PDy@Jb[]] + 1/4 Uq[] Ethbar@J[] PDy@Jb[] *)
(*				- 1/4 J[] Uqb[] Eth@PDy@Jb[] - 1/(4K[]) Eth@Uq[] Jb[] PDy[J[]Jb[]] *)
(*				- 1/(8K[]^2) Uq[]Ethbar[J[]Jb[]]PDy[J[]Jb[]] + 1/2 PDy@W[] + 1/(4R[]))*)


(* ::Input:: *)
(*scBJnValue[LI[2]]=(PDnu[R[]]/R[] scDJn[] PDy[J[]] - 1/4 W[] PDy@J[] PDy@Jb[] *)
(*				+ 1/16 W[]PDy[J[]Jb[]]^2/K[]^2)*)


(* ::Input:: *)
(*scBJnValue[LI[3]]=(-1/(8R[]) PDy@J[] PDy@Jb[] + 1/(32 R[]) PDy[J[]Jb[]]^2/K[]^2)*)


(* ::Input:: *)
(*scCJnValue=scCJValue*)


(* ::Input:: *)
(*scDJnValue=(1/4(-2 PDy[Jb[]] + Jb[]/K[]^2 PDy[J[]Jb[]]))*)


(* ::Text:: *)
(*scratch space:*)


(* ::Input:: *)
(*NewBH = W[] + 1/2 r[] PDr[W[]] + 1/2 PDr[be[]](r[] W[] + 1) - Eth@Uq[]Jb[]PDr[J[]Jb[]]r[]/4K[] + r[]Uq[]/4(Ethbar@J[] PDr[Jb[]] + Ethbar[Jb[]PDr@J[]]-Jb[]PDr@Ethbar@J[]-Ethbar[J[]Jb[]]PDr[J[]Jb[]]/2K[]^2)*)


(* ::Input:: *)
(*NewStartTerm=-1/2Eth[(J+r[]PDr@J[])Uqb[]]-1/2 PDr[r[]Ethbar@J[]]Uq[] + 1/2 PDr@PDr@J[](r[]^2 W[] + r[])*)


(* ::Input:: *)
(*(NewStartTerm//.ScalarPDToNumericalCoords//.ScalarDerivLeibniz//Expand//SimplifyWithTermSort[GatherScalarDerivFactors])/.{y[]->1-OneMinusY[]}//Expand*)


(* ::Input:: *)
(*%/.{OneMinusY[]->0}*)


(* ::Input:: *)
(*(NewBH//.ScalarPDToNumericalCoords//.ScalarDerivLeibniz//Expand//SimplifyWithTermSort[GatherScalarDerivFactors])/.{y[]->1-OneMinusY[]}//Expand*)


(* ::Input:: *)
(*%/.{OneMinusY[]->0}*)


(* ::Input:: *)
(*(%512 - W[])/OneMinusY[]//Expand*)


(* ::Input:: *)
(*((Ethbar[Jb[]PDr@J[]] - Jb[]PDr@Ethbar@J[])//.ScalarPDToNumericalCoords//.ScalarDerivLeibniz//Expand)/.{y[]->1-OneMinusY[]}//Expand*)


(* ::Subsubsection::Closed:: *)
(*Verification of condensed numerical Characteristic equations*)


(* ::Input:: *)
(*CompareEquations[SWHypersurfaceBetaNumeric,SWHypersurfaceBetaNumericCondensed]*)


(* ::Input:: *)
(*CompareEquations[SWHypersurfaceQNumeric/.SubSuplimentaryScalars,SWHypersurfaceQNumericCondensed//.SubNumericSuplimentaryScalars]//FullSimplify*)


(* ::Input:: *)
(*CompareEquations[SWHypersurfaceUNumeric/.SubSuplimentaryScalars,SWHypersurfaceUNumericCondensed/.SubSuplimentaryScalars]*)


(* ::Input:: *)
(*CompareEquations[SWHypersurfaceWNumeric/.SubSuplimentaryScalars,SWHypersurfaceWNumericCondensed/.SubNumericSuplimentaryScalars]*)


(* ::Input:: *)
(*CompareEquations[SWEvolutionNumeric//.SubSuplimentaryScalars/.{PDnu[J[]]:>H[],PDnu[Jb[]]:>Hb[]}/.VtoW//.ScalarPDToNumericalCoords,SWEvolutionNumericCondensed//.SubNumericSuplimentaryScalars]*)


(* ::Subsubsection::Closed:: *)
(*Newman-Penrose spin coefficients in Numerical coordinates*)


(* ::Input:: *)
(*qqdqTo\[CapitalTheta]=Join[MakeRule[{q[A]qb[-B]sphd[-A]@q[B],\[CapitalTheta][]}],*)
(*MakeRule[{q[A]q[B]sphd[-A]@qb[-B],-\[CapitalTheta][]}],*)
(*MakeRule[{qb[A]q[-B]sphd[-A]@qb[B],\[CapitalTheta]b[]}],*)
(*MakeRule[{qb[A]qb[B]sphd[-A]@q[-B],-\[CapitalTheta]b[]}]];*)


(* ::Input:: *)
(*Column[(#[]==Collect[((#[]/.SubNPScalars/.PDuJtoH/.VtoW//.ScalarPDToNumericalCoords//.ScalarDerivLeibniz//.qqdqTo\[CapitalTheta])/.{y[]->1-OneMinusY[]}//Expand//ScreenDollarIndices),{Exp[-2be[]],OneMinusY[],R[],1+K[]}])&/@NPScalars];*)
(*%/.{\[CapitalTheta]->Zero,\[CapitalTheta]b->Zero}*)


(* ::Subsubsection::Closed:: *)
(*Weyl scalars in numerical coordinates*)


(* ::Input:: *)
(*(WeylScalarToBondi[Psi0[]]//SimplifyWithTermSort[GatherScalarDerivFactors])//.ScalarPDToNumericalCoords//.ScalarDerivLeibniz/.{y[]->1-OneMinusY[]};*)
(*Collect[%//NoScalar,{R[],K[],OneMinusY[],PDy[be[]],PDy@PDy@_,PDy@_}]*)
(*Collect[16 R[]^2 K[]/OneMinusY[]^4 %//Expand,{PDy[be[]],PDy@PDy@_,PDy@_},Simplify]/. {K[]^2->1+J[]Conj[J[]]}*)


(* ::Input:: *)
(*EthbePlusHalfQ = Eth[be[]]+1/2 Qq[];*)
(*Psi1SimpInner=J[](-2PDy@Conj@Qq[]+PDy@Conj@J[](2EthbePlusHalfQ+J[]Conj[EthbePlusHalfQ]))+*)
(*(1+K[])(EthbePlusHalfQ(Conj[J[]]PDy[J[]]-J[]PDy[Conj[J[]]])+2(PDy@Qq[]+J[]PDy@Conj@Qq[])*)
(*-(1+K[])(2PDy@Qq[]+PDy@J[]Conj[EthbePlusHalfQ]));*)
(*Psi1Simplified=OneMinusY[]^2/(R[]^2 Sqrt[1+K[]] Sqrt[128]) (J[]Conj[EthbePlusHalfQ]-(1+K[])EthbePlusHalfQ*)
(*+OneMinusY[]((1+K[]) Eth[PDy[be[]]]- J[]Ethbar[PDy[be[]]]*)
(*+PDy[be[]]( nEthRuponR[]*(1+K[])-J[]nEthbarRuponR[])*)
(*+1/(4K[]) Psi1SimpInner))*)


(* ::Input:: *)
(*(WeylScalarToBondi[Psi1[]]//SimplifyWithTermSort[GatherScalarDerivFactors])//.ScalarPDToNumericalCoords//.ScalarDerivLeibniz/.{y[]->1-OneMinusY[]}/.nEthRToRatios;*)
(*Psi1Automatic=Collect[%//NoScalar,{R[],K[],OneMinusY[],PDy[be[]],PDy@PDy@_,PDy@_}]*)
(*Psi1Automatic-Psi1Simplified//Simplify*)


(* ::Input:: *)
(*LeafCount/@{Psi1Automatic,Psi1Simplified}*)


(* ::Input:: *)
(*(WeylScalarToBondi[Psi2[]]//SimplifyWithTermSort[GatherScalarDerivFactors])//.ScalarPDToNumericalCoords//.ScalarDerivLeibniz/.{y[]->1-OneMinusY[]}/.nEthRToRatios;*)
(*Psi2Automatic=Collect[%//NoScalar,{Exp[-2be[]],R[],K[],OneMinusY[],PDy[be[]],PDy@PDy@_,PDy@_}]*)


(* ::Section:: *)
(*Regularity preserving coordinate transformations*)


(* ::Subsection:: *)
(*Definitions for tilded Bondi-Sachs quantities*)


(* ::Subsubsection::Closed:: *)
(*xTensor object definitions*)


(* ::Text:: *)
(*4-dimensional indices and metric*)


(* ::Input:: *)
(*DefVBundle[TangentM4t, M4, 4, {at,bt,ct,dt,et,ft}];*)


(* ::Input:: *)
(*DefVBundle[TangentM2t, M2, 2, {Bt,Ct,Et,Ft,Gt,Ht}];*)


(* ::Text:: *)
(*For the moment, we do not define a tilded metric; it may not be necessary for this part of the computation.*)


(* ::Input:: *)
(*(*DefMetric[1,sphmet2[-A,-B],sphd,PrintAs\[Rule]"q"];*)*)


(* ::Text:: *)
(*The xTensor system will treat this as an additional quantity with formulaically defined derivatives and curvature, but not use it for canonicallization*)


(* ::Input:: *)
(*(*DefMetric[1,met2[-A,-B],cd,PrintAs\[Rule]"h"];*)*)


(* ::Subsubsection::Closed:: *)
(*Bondi-Sachs component definitions*)


(* ::Text:: *)
(*'up' index basis vectors for the u and r coordinates.*)


(* ::Input:: *)
(*DefTensor[uvt[at],{M4}];*)
(*DefTensor[rvt[at],{M4}];*)


(* ::Text:: *)
(*'down' index basis vectors for the u and r coordinates*)


(* ::Input:: *)
(*DefTensor[rdt[at],{M4}];*)
(*DefTensor[udt[bt],{M4}];*)


(* ::Text:: *)
(*Angular basis vectors:*)


(* ::Input:: *)
(*DefTensor[basvt[at,Bt],{M2,M4},PrintAs->"\!\(\*OverscriptBox[\(e\), \(~\)]\)"];*)
(*DefTensor[basdt[at,Bt],{M2,M4},PrintAs->"\!\(\*OverscriptBox[\(e\), \(~\)]\)"];*)


(* ::Text:: *)
(*The angular null vectors; note that the 'v' indicates the 4-component version and without represents *)
(*the 2-component version.*)


(* ::Input:: *)
(*DefTensor[qv4t[at],{M4},PrintAs->"\!\(\*OverscriptBox[\(q\), \(~\)]\)"];*)
(*DefTensor[qbv4t[at],{M4},PrintAs->"\!\(\*OverscriptBox[OverscriptBox[\(q\), \(_\)], \(~\)]\)"];*)
(*DefTensor[qd4t[at],{M2,M4},PrintAs->"\!\(\*OverscriptBox[\(q\), \(~\)]\)"];*)
(*DefTensor[qbd4t[at],{M2,M4},PrintAs->"\!\(\*OverscriptBox[OverscriptBox[\(q\), \(_\)], \(~\)]\)"];*)


(* ::Input:: *)
(*DefTensor[qt[Bt],{M2,M4},PrintAs->"\!\(\*OverscriptBox[\(q\), \(~\)]\)"];*)
(*DefTensor[qbt[Bt],{M2,M4},PrintAs->"\!\(\*OverscriptBox[OverscriptBox[\(q\), \(_\)], \(~\)]\)"];*)


(* ::Text:: *)
(*Tensor representing the Bondi-Sachs radial coordinate*)


(* ::Input:: *)
(*DefTensor[rt[],M4,PrintAs->"\!\(\*OverscriptBox[\(r\), \(~\)]\)"];*)


(* ::Text:: *)
(*Bondi-Sachs metric coefficient scalars*)


(* ::Input:: *)
(*DefTensor[bet[],{M4},PrintAs->"\!\(\*OverscriptBox[\(\[Beta]\), \(~\)]\)"];*)
(*DefTensor[Vt[],M4,PrintAs->"\!\(\*OverscriptBox[\(V\), \(~\)]\)"];*)


(* ::Text:: *)
(*Bondi-Sachs angular component tensors*)


(* ::Input:: *)
(*DefTensor[ht[Bt,Ct],{M2,M4},PrintAs->"\!\(\*OverscriptBox[\(h\), \(~\)]\)"];*)
(*DefTensor[Ut[Bt],M4,PrintAs->"\!\(\*OverscriptBox[\(U\), \(~\)]\)"];*)


(* ::Input:: *)
(*DefTensor[Qt[A],M4,PrintAs->"\!\(\*OverscriptBox[\(Q\), \(~\)]\)"];*)


(* ::Text:: *)
(*Spin-weighted scalars for angular components*)


(* ::Input:: *)
(*DefTensor[Uqt[],{M4},PrintAs->"\!\(\*OverscriptBox[SubscriptBox[\(U\), \(q\)], \(~\)]\)"];*)
(*DefTensor[Uqbt[],{M4},PrintAs->"\!\(\*OverscriptBox[SubscriptBox[\(U\), OverscriptBox[\(q\), \(_\)]], \(~\)]\)"];*)


(* ::Input:: *)
(*DefTensor[Qqt[],{M4},PrintAs->"\!\(\*OverscriptBox[SubscriptBox[\(Q\), \(q\)], \(~\)]\)"];*)
(*DefTensor[Qqbt[],{M4},PrintAs->"\!\(\*OverscriptBox[SubscriptBox[\(Q\), OverscriptBox[\(q\), \(_\)]], \(~\)]\)"];*)


(* ::Input:: *)
(*DefTensor[Jt[],{M4},PrintAs->"\!\(\*OverscriptBox[\(J\), \(~\)]\)"];*)
(*DefTensor[Jbt[],{M4},PrintAs->"\!\(\*OverscriptBox[OverscriptBox[\(J\), \(_\)], \(~\)]\)"];*)


(* ::Input:: *)
(*DefTensor[Kt[],{M4},PrintAs->"\!\(\*OverscriptBox[\(K\), \(~\)]\)"];*)
(*DefTensor[OnePlusKt[],{M4},PrintAs->"(1+\!\(\*OverscriptBox[\(K\), \(~\)]\))"];*)


(* ::Subsubsection::Closed:: *)
(*Coordinate transformation definitions*)


(* ::Text:: *)
(*Jacobians for the transformation*)


(* ::Input:: *)
(*DefTensor[dxtdx[at,a],{M4},PrintAs->"d\!\(\*OverscriptBox[\(x\), \(~\)]\)/dx"];*)
(*DefTensor[dxdxt[a,at],{M4},PrintAs->"dx/d\!\(\*OverscriptBox[\(x\), \(~\)]\)"];*)


(* ::Text:: *)
(*Angular part for convenience*)


(* ::Input:: *)
(*DefTensor[dXtdX[Bt,A],{M4,M2},PrintAs->"d\!\(\*OverscriptBox[\(X\), \(~\)]\)/dX"];*)


(* ::Input:: *)
(*DefTensor[dXdXt[A,Bt],{M4,M2},PrintAs->"dX/d\!\(\*OverscriptBox[\(X\), \(~\)]\)"];*)


(* ::Input:: *)
(*DefTensor[jaca[],{M4,M2},PrintAs->"a"];*)
(*DefTensor[jacb[],{M4,M2},PrintAs->"b"];*)
(*DefTensor[jacab[],{M4,M2},PrintAs->"\!\(\*OverscriptBox[\(a\), \(_\)]\)"];*)
(*DefTensor[jacbb[],{M4,M2},PrintAs->"\!\(\*OverscriptBox[\(b\), \(_\)]\)"];*)


(* ::Input:: *)
(*DefTensor[jacc[],{M4,M2},PrintAs->"c"];*)
(*DefTensor[jacd[],{M4,M2},PrintAs->"d"];*)
(*DefTensor[jaccb[],{M4,M2},PrintAs->"\!\(\*OverscriptBox[\(c\), \(_\)]\)"];*)
(*DefTensor[jacdb[],{M4,M2},PrintAs->"\!\(\*OverscriptBox[\(d\), \(_\)]\)"];*)


(* ::Text:: *)
(*The coordinate transformations we consider are constant in r:*)


(* ::Input:: *)
(*dxtdx/:dxtdx[at_,-a_]basdt[-at_,Bt_]rv[a_]:=0;*)


(* ::Input:: *)
(*dxtdx/:dxtdx[at_,-a_]basdt[-at_,Bt_]basv[a_,A_]:=dXtdX[Bt,A];*)


(* ::Input:: *)
(*rv/:PD[c_][dxtdx[at_,-a_]]basdt[-at_,Bt_]rv[a_]:=0;*)


(* ::Text:: *)
(*And we do not alter the time component:*)


(* ::Input:: *)
(*dxtdx/:dxtdx[at_,-a_]udt[-at_]uv[a_]:=1;*)
(*dxtdx/:dxtdx[at_,-a_]udt[-at_]rv[a_]:=0;*)
(*dxtdx/:dxtdx[at_,-a_]udt[-at_]basv[a_,A_]:=0;*)


(* ::Input:: *)
(*DefTensor[OmegaCD[],{M4},PrintAs->"\[Omega]"];*)


(* ::Input:: *)
(*DefTensor[U0[A],{M4,M2},PrintAs->"\!\(\*SubscriptBox[\(U\), \(0\)]\)"];*)
(*DefTensor[U0q[A],{M4,M2},PrintAs->"\!\(\*SubscriptBox[\(U\), \(0  q\)]\)"];*)
(*DefTensor[U0qb[A],{M4,M2},PrintAs->"\!\(\*SubscriptBox[\(U\), \(0 \*OverscriptBox[\(q\), \(_\)]\)]\)"];*)


(* ::Input:: *)
(*dxtdx/:dxtdx[at_,-a_]rdt[-at_]rv[a_]:=OmegaCD[];*)
(*dxtdx/:dxtdx[at_,-a_]rdt[-at_]uv[a_]:=r[]PDu[OmegaCD[]];*)


(* ::Input:: *)
(*dxtdx/:dxtdx[at_,-a_]rdt[-at_]basv[a_,A_]:=r[]PD[A][OmegaCD[]];*)


(* ::Input:: *)
(*dxtdx/:dxtdx[at_,-a_]basdt[-at_,Bt_]uv[a_]:=Module[{A},-dxtdx[at,-a]basdt[-at,Bt]basv[a,-A]U0[A]];*)


(* ::Input:: *)
(*sphdToEth=Flatten@MapThread[GenerateSphdSubstitutions,*)
(*Transpose@{*)
(*{be[],{{be[],1}}},*)
(*{OmegaCD[],{{OmegaCD[],1}}},*)
(*{V[], {{V[],1}}},*)
(*{U[A_], {{Uq[],(1/2)qb[A]},{Uqb[],(1/2)q[A]}}},*)
(*{U0[A_], {{U0q[],(1/2)qb[A]},{U0qb[],(1/2)q[A]}}},*)
(*{Q[A_], {{Qq[],(1/2)qb[A]},{Qqb[],(1/2)q[A]}}},*)
(*{met2[-A_,-B_],*)
(*{{J[],(1/2)qb[-A]qb[-B]},{Jb[],(1/2)q[-A]q[-B]},*)
(*{Sqrt[1 + J[]Jb[]],(1/2)(q[-A]qb[-B] + qb[-A]q[-B])}}}*)
(*}];*)


(* ::Input:: *)
(*Utoq={U[A_]:>((1/2)qb[A]Uq[] + (1/2)q[A]Uqb[]),*)
(*	U0[A_]:>((1/2)qb[A]U0q[] + (1/2)q[A]U0qb[]),*)
(*	U0h[At_]:>((1/2)qbt[At]U0qh[] + (1/2)qt[At]U0qbh[])};*)


(* ::Input:: *)
(*dXtdX/:qt[-Bt_]dXtdX[Bt_,-A_]q[A_]:=jaca[];*)
(*dXtdX/:qt[-Bt_]dXtdX[Bt_,-A_]qb[A_]:=jacb[];*)
(*dXtdX/:qbt[-Bt_]dXtdX[Bt_,-A_]qb[A_]:=jacab[];*)
(*dXtdX/:qbt[-Bt_]dXtdX[Bt_,-A_]q[A_]:=jacbb[];*)


(* ::Input:: *)
(*dXdXt/:q[-B_]dXdXt[B_,-Bt_]qt[Bt_]:=jacc[];*)
(*dXdXt/:q[-B_]dXdXt[B_,-Bt_]qbt[Bt_]:=jacd[];*)
(*dXdXt/:qb[-B_]dXdXt[B_,-Bt_]qbt[Bt_]:=jaccb[];*)
(*dXdXt/:qb[-B_]dXdXt[B_,-Bt_]qt[Bt_]:=jacdb[];*)


(* ::Input:: *)
(*AngularIdentityTest=(1/2)dXdXt[A,-Bt](qt[Bt]qbt[-Ct] + qbt[Bt]qt[-Ct])dXtdX[Ct,-C]//Expand*)


(* ::Input:: *)
(*jacabTojaccd=Solve[{(q[-A]AngularIdentityTest q[C]//Expand)==0,*)
(*(q[-A]AngularIdentityTest qb[C]//Expand)==2,*)
(*(qb[-A]AngularIdentityTest q[C]//Expand)==2,*)
(*(qb[-A]AngularIdentityTest qb[C]//Expand)==0},{jaca[],jacb[],jacab[],jacbb[]}][[1]]*)


(* ::Input:: *)
(*rTort={r[]->rt[]/OmegaCD[]};*)
(*rtTor={rt[]->r[]*OmegaCD[]};*)


(* ::Input:: *)
(*detToOmegaCD={jacd[]jacdb[]-jacc[]jaccb[]:>4*OmegaCD[]^2,*)
(*-jacd[]jacdb[]+jacc[]jaccb[]:>-4*OmegaCD[]^2}*)


(* ::Input:: *)
(*DefInertHead[PDrt,PrintAs->"\!\(\*SubscriptBox[\(\[PartialD]\), OverscriptBox[\(r\), \(~\)]]\)"];*)
(*DefInertHead[PDut,PrintAs->"\!\(\*SubscriptBox[\(\[PartialD]\), OverscriptBox[\(u\), \(~\)]]\)"];*)


(* ::Input:: *)
(*DefInertHead[Etht,PrintAs->"\!\(\*OverscriptBox[\(\[PartialD]\), \(~\)]\)"];*)
(*DefInertHead[Ethbart,PrintAs->"\!\(\*OverscriptBox[OverscriptBox[\(\[PartialD]\), \(_\)], \(~\)]\)"];*)


(* ::Input:: *)
(*ScalarPDList={PDr,PDu,Eth,Ethbar,PDrt,PDut,Etht, Ethbart,PDy,PDnu,nEth,nEthbar};*)


(* ::Subsubsection::Closed:: *)
(*Coordinate transformation derivations*)


(* ::Input:: *)
(**)


(* ::Text:: *)
(*Confirmation of vanishing parts of the Bondi-like metric:*)


(* ::Input:: *)
(*(udt[-at]udt[-bt]dxtdx[at,-a]dxtdx[bt,-b]met[a,b])/.MetricValues//ExpandAll*)


(* ::Input:: *)
(*(basdt[-at,Bt]udt[-bt]dxtdx[at,-a]dxtdx[bt,-b]met[a,b])/.MetricValues//ExpandAll*)


(* ::Text:: *)
(*"Lapse" part*)


(* ::Input:: *)
(*(udt[-at]rdt[-bt]dxtdx[at,-a]dxtdx[bt,-b]met[a,b])/.MetricValues//ExpandAll*)


(* ::Input:: *)
(*betValue=be[]-(1/2)Log[OmegaCD[]];*)


(* ::Input:: *)
(*beTobet={be[]->bet[]+(1/2)Log[OmegaCD[]]}*)


(* ::Text:: *)
(*Angular part:*)


(* ::Input:: *)
(*(((qt[-Bt]basdt[-at,Bt]qt[-Ct]basdt[-bt,Ct]dxtdx[at,-a]dxtdx[bt,-b]met[a,b])/.MetricValues//ExpandAll*)
(*//SimplifyEFEComponent//TensorExpressionToSimplifiedSWScalar//Expand//Simplify)/.jacabTojaccd//FullSimplify)/.rTort/.detToOmegaCD*)


(* ::Input:: *)
(*(((qt[-Bt]basdt[-at,Bt]qbt[-Ct]basdt[-bt,Ct]dxtdx[at,-a]dxtdx[bt,-b]met[a,b])/.MetricValues//ExpandAll*)
(*//SimplifyEFEComponent//TensorExpressionToSimplifiedSWScalar//Expand//Simplify)/.jacabTojaccd//FullSimplify)/.rTort/.detToOmegaCD*)


(* ::Text:: *)
(*Recall original angular part:*)


(* ::Input:: *)
(*(q[-A]q[-B]basd[-a,A]basd[-b,B]met[a,b])/.MetricValues//Expand//TensorExpressionToSimplifiedSWScalar*)


(* ::Input:: *)
(*(q[-A]qb[-B]basd[-a,A]basd[-b,B]met[a,b])/.MetricValues//Expand//TensorExpressionToSimplifiedSWScalar*)


(* ::Text:: *)
(*So, we infer:*)


(* ::Input:: *)
(*JtValue=(1/4)(jacdb[]^2 J[] + jacc[]^2Jb[]+ 2 jacc[]jacdb[]K[])/OmegaCD[]^2;*)


(* ::Input:: *)
(*JbtValue=(1/4)(jacd[]^2 Jb[] + jaccb[]^2J[]+ 2 jaccb[]jacd[]K[])/OmegaCD[]^2;*)


(* ::Input:: *)
(*KbtValue=(1/4)(jacdb[]jaccb[]J[]+jacc[]jacd[]Jb[]+(jacc[]jaccb[]+jacd[]jacdb[])K[])/OmegaCD[]^2;*)


(* ::Input:: *)
(*Conj[jacc[]]:=jaccb[];Conj[jacd[]]:=jacdb[];Conj[jaccb[]]:=jacc[];Conj[jacdb[]]:=jacd[];*)
(*Conj[OmegaCD[]]:=OmegaCD[];*)


(* ::Input:: *)
(*JtoJt={J[]->(OmegaCD[]^2/4)(jacbb[]^2Jt[]+jaca[]^2Jbt[]+2jaca[]jacbb[]Kt[]),*)
(*	Jb[]->(OmegaCD[]^2/4)(jacb[]^2Jbt[]+jacab[]^2Jt[]+2jacab[]jacb[]Kt[]),*)
(*K[]->(OmegaCD[]^2/4)*(jacab[]jacbb[]Jt[]+jaca[]jacb[]Jbt[]+(jaca[]jacab[]+jacb[]jacbb[])Kt[])};*)


(* ::Text:: *)
(*Checking:*)


(* ::Input:: *)
(*((OmegaCD[]^2/4)(jacbb[]^2Jt[]+jaca[]^2Jbt[]+2jaca[]jacbb[]Kt[])/.jacabTojaccd//Expand//FullSimplify)/.detToOmegaCD*)


(* ::Input:: *)
(*InputForm[%819]*)


(* ::Input:: *)
(*(JtValue)/.JtoJt/.jacabTojaccd//Expand//FullSimplify*)


(* ::Input:: *)
(*(JbtValue)/.JtoJt/.jacabTojaccd//Expand//FullSimplify*)


(* ::Input:: *)
(*KbtValue/.JtoJt/.jacabTojaccd//Expand//FullSimplify*)


(* ::Text:: *)
(*Establishing these relationships is important for the subsequent steps. By necessity of maintaining representability, we will often need to express*)
(*The new components in terms of the new J, as that is what will be evolved.*)


(* ::Text:: *)
(*We will also on a case-by-case basis wish to transform certain spin-weighted derivatives from one basis to another:*)


(* ::Text:: *)
(*(TODO make this process a bit more robust)*)


(* ::Input:: *)
(*((1/2)q[B]dXtdX[Bt,-B](qt[-Bt]qbt[Ct]+qbt[-Bt]q[Ct])PD[-Ct][OmegaCD[]]//Expand//TensorExpressionToSimplifiedSWScalar)/.jacabTojaccd/.detToOmegaCD//Simplify*)


(* ::Text:: *)
(*So, we infer:*)


(* ::Input:: *)
(*EthOmegaToTilded={Eth[OmegaCD[]]:>(1/(2OmegaCD[]^2))(jacd[]Etht[OmegaCD[]]-jacc[]Ethbart[OmegaCD[]]),*)
(*				Ethbar[OmegaCD[]]:>(1/(2OmegaCD[]^2))(jacdb[]Ethbart[OmegaCD[]]-jaccb[]Etht[OmegaCD[]])}*)


(* ::Text:: *)
(*Note that angular part of the metric determines the radial coordinate:*)


(* ::Input:: *)
(*(1/4)*(q[-A]qb[-B]basd[-a,A]basd[-b,B]met[a,b] * (qb[-C]q[-D]basd[-c,C]basd[-d,D]met[c,d])-*)
(*q[-A]q[-B]basd[-a,A]basd[-b,B]met[a,b] * (qb[-C]qb[-D]basd[-c,C]basd[-d,D]met[c,d]))/.MetricValues//Expand//TensorExpressionToSimplifiedSWScalar*)


(* ::Input:: *)
(*AngularTransformedMetric[Bt_,Ct_]:=Module[{a,b,at,bt},basdt[-at,Bt]basdt[-bt,Ct]dxtdx[at,-a]dxtdx[bt,-b]met[a,b]];*)


(* ::Input:: *)
(*invrtilde4=(1/4)((AngularTransformedMetric[Bt,Ct]qt[-Bt]qbt[-Ct] * AngularTransformedMetric[Et,Ft]qt[-Et]qbt[-Ft]*)
(*-AngularTransformedMetric[Bt,Ct]qt[-Bt]qt[-Ct] * AngularTransformedMetric[Et,Ft]qbt[-Et]qbt[-Ft])/.MetricValues//TensorExpressionToSimplifiedSWScalar//Simplify)*)


(* ::Input:: *)
(*invrtilde4/.jacabTojaccd//Simplify*)


(* ::Text:: *)
(*Therefore, because \[Omega]=d \!\(\*OverscriptBox[\(r\), \(~\)]\)/dr, we infer*)


(* ::Input:: *)
(*OmegaCDtoab={OmegaCD[]->2/Sqrt[-jaca[]*jacab[]+jacb[]*jacbb[]]};*)


(* ::Input:: *)
(*OmegaCDtocd={OmegaCD[]->Sqrt[-jacc[]*jaccb[]+jacd[]*jacdb[]]/2};*)


(* ::Text:: *)
(*"Shift" part:*)


(* ::Input:: *)
(*((((qt[-Bt]basdt[-at,Bt]rdt[-bt]dxtdx[at,-a]dxtdx[bt,-b]met[a,b])/.MetricValues//ExpandAll*)
(*//SimplifyEFEComponent//TensorExpressionToSimplifiedSWScalar//Expand//Simplify)*)
(*/.jacabTojaccd//FullSimplify)/.rTort/.detToOmegaCD/.JtoKRule/.JtoJt/.jacabTojaccd/.EthOmegaToTilded*)
(*		//ExpandAll//Simplify)/.detToOmegaCD//Simplify*)


(* ::Text:: *)
(*Comparing against the original:*)


(* ::Input:: *)
(*(q[-B]basd[-a,B]rd[-b]met[a,b])/.MetricValues//SimplifyEFEComponent//TensorExpressionToSimplifiedSWScalar*)


(* ::Input:: *)
(*UtValue=1/(2OmegaCD[]^2) *(jacdb[](Uq[]-U0q[])-jacc[](Uqb[]-U0qb[]))-Exp[2bet[]](Etht[OmegaCD[]]Kt[] - Ethbart[OmegaCD[]]Jt[])/(OmegaCD[]rt[])*)


(* ::Input:: *)
(*(E^(2 bet[]) (-Ethbart[OmegaCD[]] Jt[]+Etht[OmegaCD[]] Kt[]))/(OmegaCD[] rt[])*)


(* ::Input:: *)
(*UbtValue=1/(2OmegaCD[]^2) *(jacd[](Uqb[]-U0qb[])-jaccb[](Uq[]-U0q[]))-Exp[2bet[]](Ethbart[OmegaCD[]]Kt[] - Etht[OmegaCD[]]Jbt[])/(OmegaCD[]rt[])*)


(* ::Input:: *)
(*(-Exp[-2bet[]]Uqt[])//.{bet[]->betValue,Uqt[]->UtValue}//ExpandAll*)


(* ::Input:: *)
(*UtoUt={Uq[]:>(OmegaCD[]^2/2)(jacbb[](Uqt[])-jaca[](Uqbt[]))+U0q[] *)
(*			- Exp[2bet[]]OmegaCD[]/(2rt[])(jacbb[](Ethbart[OmegaCD[]]Jt[]-Etht@OmegaCD[]Kt[])-jaca[]*(Etht[OmegaCD[]]Jbt[]-Ethbart@OmegaCD[]Kt[])),*)
(*	Uqb[]:>(OmegaCD[]^2/2)(jacb[](Uqbt[])-jacab[](Uqt[])) + U0qb[]*)
(*			- Exp[2bet[]]OmegaCD[]/(2rt[])(jacb[]*(Etht[OmegaCD[]]Jbt[]-Ethbart@OmegaCD[]Kt[])-jacab[]*(Ethbart[OmegaCD[]]Jt[]-Etht@OmegaCD[]Kt[]))}*)


(* ::Input:: *)
(*(UtValue/.UtoUt/.jacabTojaccd//Expand//Simplify)/.detToOmegaCD//Expand*)


(* ::Input:: *)
(*(UbtValue/.UtoUt/.jacabTojaccd//Expand//Simplify)/.detToOmegaCD//Expand*)


(* ::Input:: *)
(*PDrt[U0q[]]:=0;PDrt[U0qb[]]:=0;*)
(*PDrt[rt[]]:=1;*)


(* ::Input:: *)
(*PDrt[OmegaCD[]]:=0;*)
(*PDrt[Etht@OmegaCD[]]:=0;*)
(*PDrt[Ethbart@OmegaCD[]]:=0;*)
(*PDrt[jacc[]]:=0;*)
(*PDrt[jacd[]]:=0;*)
(*PDrt[jaccb[]]:=0;*)
(*PDrt[jacdb[]]:=0;*)


(* ::Text:: *)
(*\!\(\*OverscriptBox[\(Q\), \(~\)]\) is defined via \!\( *)
(*\*SubscriptBox[\(\[PartialD]\), *)
(*OverscriptBox[\(r\), \(~\)]]*)
(*\*OverscriptBox[\(U\), \(~\)]\):*)


(* ::Input:: *)
(*betEOM={PDrt[bet[]]:>((SWHypersurfaceBetaCondensed[[2]])/.{PDr->PDrt,r->rt,J->Jt,Jb->Jbt})};*)


(* ::Input:: *)
(*rtTor*)


(* ::Input:: *)
(*PDrtToPDr={PDrt[exp_]->PDr[exp]/OmegaCD[]};*)


(* ::Input:: *)
(*((PDrt[UtValue]//.ScalarDerivLeibniz//Expand)/.betEOM/.{PDrt[Kt[]]:>PDrt[Sqrt[1+Jt[]Jbt[]]]}//.ScalarDerivLeibniz)/.{PDrt[exp_]/;!FreeQ[exp,Uq|Uqb]:>PDr[exp]/OmegaCD[]}*)


(* ::Text:: *)
(*"Mass aspect" part*)


(* ::Input:: *)
(*(((rdt[-at]rdt[-bt]dxtdx[at,-a]dxtdx[bt,-b]met[a,b])/.MetricValues*)
(*	//ExpandAll//SimplifyEFEComponent//TensorExpressionToSimplifiedSWScalar)/.VtoW/.rTort//Expand)*)


(* ::Input:: *)
(*(((rdt[-at]rdt[-bt]dxtdx[at,-a]dxtdx[bt,-b]met[a,b])/.MetricValues*)
(*	//ExpandAll//SimplifyEFEComponent//TensorExpressionToSimplifiedSWScalar)/.VtoW/.rTort/.JtoKRule//Expand)*)


(* ::Input:: *)
(*DefTensor[Uqh[],{M4},PrintAs->"\!\(\*SubscriptBox[OverscriptBox[\(U\), \(^\)], \(q\)]\)"];*)
(*DefTensor[Uqbh[],{M4},PrintAs->"\!\(\*SubscriptBox[OverscriptBox[\(U\), \(^\)], OverscriptBox[\(q\), \(_\)]]\)"];*)


(* ::Input:: *)
(*DefTensor[U0h[Bt],{M4},PrintAs->"\!\(\*SubscriptBox[OverscriptBox[\(U\), \(^\)], \(0  q\)]\)"];*)
(*DefTensor[U0qh[],{M4},PrintAs->"\!\(\*SubscriptBox[OverscriptBox[\(U\), \(^\)], \(0  q\)]\)"];*)
(*DefTensor[U0qbh[],{M4},PrintAs->"\!\(\*SubscriptBox[OverscriptBox[\(U\), \(^\)], \(0 \*OverscriptBox[\(q\), \(_\)]\)]\)"];*)


(* ::Input:: *)
(*UtoUh*)


(* ::Input:: *)
(*(jacbb[] Uqh[]-jaca[] Uqbh[])//InputForm*)


(* ::Input:: *)
(*UtoUh={Uq[]:>(OmegaCD[]^2/2)(jacbb[](Uqh[])-jaca[](Uqbh[]))*)
(*			- Exp[2bet[]]OmegaCD[]/(2rt[])(jacbb[](Ethbart[OmegaCD[]]Jt[]-Etht@OmegaCD[]Kt[])-jaca[]*(Etht[OmegaCD[]]Jbt[]-Ethbart@OmegaCD[]Kt[])),*)
(*	Uqb[]:>(OmegaCD[]^2/2)(jacb[](Uqbh[])-jacab[](Uqh[])) *)
(*			- Exp[2bet[]]OmegaCD[]/(2rt[])(jacb[]*(Etht[OmegaCD[]]Jbt[]-Ethbart@OmegaCD[]Kt[])-jacab[]*(Ethbart[OmegaCD[]]Jt[]-Etht@OmegaCD[]Kt[]))}*)


(* ::Input:: *)
(*((((rdt[-at]rdt[-bt]dxtdx[at,-a]dxtdx[bt,-b]met[a,b])/.MetricValues*)
(*	//ExpandAll//SimplifyEFEComponent//TensorExpressionToSimplifiedSWScalar)/.VtoW/.rTort//Expand)*)
(*/.EthOmegaToTilded/.JtoKRule//.JtoJt//.UtoUh/.jacabTojaccd/.beTobet//Simplify)//Expand*)


(* ::Text:: *)
(*Comparing against the original expression:*)


(* ::Input:: *)
(*((rd[-a]rd[-b]met[a,b])/.MetricValues//SimplifyEFEComponent//TensorExpressionToSimplifiedSWScalar)/.VtoW//Expand*)


(* ::Input:: *)
(*DefTensor[Wt[],{M4},PrintAs->"\!\(\*OverscriptBox[\(W\), \(~\)]\)"];*)


(* ::Input:: *)
(*WtValue=(W[]+(1/rt[])(OmegaCD[]-1) *)
(*+ Exp[2bet[]]/(2 OmegaCD[]^2rt[])((Etht@OmegaCD[])^2Jbt[] + (Ethbart@OmegaCD[])^2Jt[]-2*Etht@OmegaCD[]Ethbart@OmegaCD[]Kt[])*)
(*- 1/OmegaCD[](2PDu[OmegaCD[]]+Uqbh[]Etht[OmegaCD[]]+Uqh[]Ethbart[OmegaCD[]]))*)


(* ::Input:: *)
(*(E^(2 bet[]) (Etht[OmegaCD[]]^2 Jbt[]+Ethbart[OmegaCD[]]^2 Jt[]-2 Ethbart[OmegaCD[]] Etht[OmegaCD[]] Kt[]))/(2 OmegaCD[]^2 rt[])*)


(* ::Input:: *)
(*((Exp[-2bet[]]+Exp[-2bet[]]rt[]Wt[])/.{Wt[]->WtValue}//Expand)*)


(* ::Text:: *)
(*H transformation*)


(* ::Input:: *)
(*r[]^-2(1/2)q[A]q[B]basv[a,-A]basv[b,-B]met[-a,-b]/.MetricValues//Expand//TensorExpressionToSimplifiedSWScalar*)


(* ::Input:: *)
(*dxdxt[c,-at](-ud[-c]uv[d]+rd[-c]rv[d] + basd[-c,A]basv[d,-A])*)


(* ::Input:: *)
(*uvt/:uvt[at_]PD[-at_][basdt[-ct_,C_]]:=0;*)
(*uvt/:uvt[at_]PD[-at_][basvt[ct_,-C_]]:=0;*)


(* ::Input:: *)
(*((rt[]^-2(1/2)uvt[at]qt[Bt]qt[Ct]PD[-at][1/(OmegaCD[]^2)basvt[bt,-Bt]basvt[ct,-Ct]dxdxt[a,-bt]dxdxt[b,-ct]met[-a,-b]]//ExpandAll)\*)
(*/.{uvt[at_]PD[-at_][met[a_,b_]]:>Module[{d,e,A},uvt[at]dxdxt[d,-at](-ud[-d]uv[e]+rd[-d]rv[e] + basd[-d,A]basv[e,-A])PD[-e][met[a,b]]]}//Expand)*)


(* ::Input:: *)
(*((((-rt[]^2(1/2)qt[-Bt]qt[-Ct]basdt[-bt,Bt]basdt[-ct,Ct]dxtdx[bt,-a]dxtdx[ct,-b]met[a,b])/.MetricValues//Expand//TensorExpressionToSimplifiedSWScalar)/.JtoKRule/.JtoJt//Expand)/.jacabTojaccd//Simplify)/.rTort/.detToOmegaCD*)


(* ::Text:: *)
(*Note this is just du[Jtilde], so requires a simple correction to dutilde[jtilde]*)


(* ::Input:: *)
(*((-(1/2)uv[d]qt[-Bt]qt[-Ct]basdt[-bt,Bt]basdt[-ct,Ct]PD[-d][rt[]^2dxtdx[bt,-a]dxtdx[ct,-b]met[a,b]]//ExpandAll)\*)
(*/.{uvt[at_]PD[-at_][dxtdx[bt_,c_]]:>Module[{d,e,A},uvt[at]dxdxt[d,-at](-ud[-d]uv[e]+rd[-d]rv[e] + basd[-d,A]basv[e,-A])PD[-e][dxtdx[bt,c]]],*)
(*uvt[at_]PD[-at_][met[a_,b_]]:>Module[{d,e,A},uvt[at]dxdxt[d,-at](-ud[-d]uv[e]+rd[-d]rv[e] + basd[-d,A]basv[e,-A])PD[-e][met[a,b]]]}//Expand)*)


(* ::Input:: *)
(*uvt/:uvt[at_]dxdxt[a_,-at_]basd[-a_,A_]:=U0[A]*)
(*uvt/:rvt[at_]dxdxt[a_,-at_]rd[-a_]:=1/OmegaCD[];*)
(*uvt/:uvt[at_]dxdxt[a_,-at_]rd[-a_]:=-r[]PDut[OmegaCD[]]/(OmegaCD[]^2);*)
(*uvt/:uvt[at_]dxdxt[a_,-at_]ud[-a_]:=1;*)


(* ::Input:: *)
(*uvt/:uvt[at_]basv[bt_,-B_]basd[-a_,C_]PD[-at_][dxdxt[a_,-bt_]]:=U0[Ct]*)


(* ::Input:: *)
(*rv/:rv[a_]basv[b_,A_]basdt[-at_,Bt_]PD[-a_][dxtdx[at_,-b_]]:=0;*)
(*basv/:basv[b_,A_]basdt[-at_,Bt_]PDr[dxtdx[at_,-b_]]:=0;*)


(* ::Input:: *)
(*uv/:basv[b_,A_]basdt[-bt_,Bt_]uv[a_]PD[-a_][dxtdx[bt_,-b_]]:=basv[b,A]PD[-b][U0h[Bt]];*)


(* ::Input:: *)
(*qt/:qt[-Ct_]qt[Ct_]:=0;*)
(*qt/:qbt[-Ct_]qt[Ct_]:=2;*)
(*qt/:qt[-Ct_]qbt[Ct_]:=2;*)


(* ::Input:: *)
(*rvt/:rvt[at_]PD[-at_][U0h[_]]:=0*)


(* ::Input:: *)
(*((%724/.MetricValues)//Expand)/.{basv[a_,A_]PD[-a_][U0h[Bt_]]:>Module[{at,bt,Ct},basv[a,A]dxtdx[at,-a]*(-udt[-at]uvt[bt]+rdt[-at]rvt[bt]+basdt[-at,Ct]basvt[bt,-Ct])PD[-bt][U0h[Bt]]]}//Expand*)


(* ::Input:: *)
(*ConvertAngularDerivsOfU0h=*)
(*{dXtdX[Ct_,B_]PD[-Ct_][U0h[Bt_]]:>PD[B][U0h[Bt]]}*)


(* ::Input:: *)
(*DeConvertAngularDerivsOfU0h=*)
(*{sphd[-C_][U0h[Bt_]]:>Module[{Ct},dXtdX[Ct,-C](1/4)*(qt[-Ct]qt[Bt]Ethbart@U0qbh[]+qbt[-Ct]qt[Bt]Etht@U0qbh[]+qt[-Ct]qbt[Bt]Ethbart@U0qh[]+qbt[-Ct]qbt[Bt]Eth@U0qh[])]}*)


(* ::Input:: *)
(*PDu[rt[]]:=PDu[rt[]/.rtTor]*)


(* ::Text:: *)
(*Examining all but the first two terms:*)


(* ::Input:: *)
(*(((((J[] jacb[]^2+jaca[] (jaca[] Jb[]-2 jacb[] Sqrt[1+J[] Jb[]])) PDu[OmegaCD[]] rt[])/(2 r[])+(Eth[U0qh[]] (J[] jacab[] jacb[]+jaca[] jacbb[] Jb[]-(jaca[] jacab[]+jacb[] jacbb[]) Sqrt[1+J[] Jb[]]) rt[]^2)/(4 r[]^2)+(Ethbart[U0qh[]] (J[] jacb[]^2+jaca[] (jaca[] Jb[]-2 jacb[] Sqrt[1+J[] Jb[]])) rt[]^2)/(4 r[]^2))/.JtoKRule/.JtoJt//Expand//Simplify)/.jacabTojaccd/.detToOmegaCD//Simplify)/.detToOmegaCD/.rTort//Expand*)


(* ::Text:: *)
(*And the last term:*)


(* ::Input:: *)
(*(Hb[] jaca[] (jaca[]-(J[] jacb[])/Sqrt[1+J[] Jb[]]) rt[]^2)/(4 r[]^2)+(H[] jacb[] (jacb[]-(jaca[] Jb[])/Sqrt[1+J[] Jb[]]) rt[]^2)/(4 r[]^2)/.jacabTojaccd/.detToOmegaCD/.rTort//Simplify*)


(* ::Input:: *)
(*qt[-Ct] qt[Ct]//InputForm*)


(* ::Input:: *)
(*(dXtdX[Bt, -A] dXtdX[Et, -C] Ethbart[U0qbh[]] met2[A, B] qt[-Bt] qt[-Ct] qt[Ct] qt[-Et] rt[]^2)/(4 OmegaCD[]^2 r[]^2)//IndicesOf[Free]*)


(* ::Input:: *)
(*ChangeCovD[xAct`xTensor`PD[-Ct][U0h[Et]],PD,sphd]*)


(* ::Input:: *)
(*uvt[at]PD[-at][basdt[-ct,C]]*)


(* ::Input:: *)
(*InputForm@%*)


(* ::Input:: *)
(*(basd[a, A] basv[b, -A] dxdxt[-a, -at] jaca[]^2 Jb[] rt[]^2 uvt[at] xAct`xTensor`PD[-b][OmegaCD[]])/(2 OmegaCD[]^3 r[]^2)//InputForm*)



