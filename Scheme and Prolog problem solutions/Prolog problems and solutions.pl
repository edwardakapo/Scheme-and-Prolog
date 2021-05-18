%Name: Oluwademilade Edward Akapo
%Student number: 101095403

%Our network
edge(toronto, montreal,	    505.48).
edge(vancouver, calgary,    676.14 ).
edge(ottawa, edmonton, 	    2836.48).
edge(mississauga, winnipeg,	1500.94).
edge(hamilton, 	quebec, 	792.82).
edge(brampton, kitchener, 	63.3).
edge(surrey, laval, 	    3652.17).
edge(london, victoria, 	    3284.25).
edge(halifax, markham, 	    1248.92).
edge(oshawa, gatineau, 	    304.38).
edge(vaughan, longueuil, 	511.46).
edge(windsor, burnaby, 	    3152.44).
edge(saskatoon, barrie,     2156.81).
edge(regina, richmond, 	    1331.75).
edge(oakville, 	burlington, 14.34).
edge(richmondHill, 	sherbrooke, 621.01).
edge(saguenay, 	stCatharines, 	861.28).
edge(montreal, regina,      2357.61).
edge(stCatharines, stJohns, 2119.33).
edge(waterloo, levis,       820.92).
edge(coquitlam, oakville,    3322.22).
edge(london, terrebonne,    675.47).
edge(thunderBay, kelowna, 	2185.74).
edge(stJohns, saintJohn,    1048.48).
edge(waterloo, 	terrebonne, 597.7).
edge(brantford, ajax, 	    127.48).
edge(ottawa, thunderBay,    1383.57).
edge(levis, troisRivieres, 	116.28).
edge(abbotsford, cambridge, 3244.06).
edge(kingston, 	guelph, 	311.27).
edge(coquitlam, whitby, 	3353.39).
edge(kelowna, laval,        4186.07).
edge(oakville, london,      155.94 ).
edge(cambridge, edmonton,   3385.31 ).
edge(troisRivieres, surrey, 4660.99 ).
edge(victoria,halifax,      6207.31 ).
edge(guelph, waterloo,      23.73 ).
edge(kelowna, gatineau,     3265.9).
edge(stJohns, laval,        1618.64).
edge(barrie,oshawa,         85.46 ).
edge(edmonton, ottawa,      2836.48).
edge(winnipeg, halifax,     2575.26 ).
edge(laval, hamilton,       550.99).
edge(richmond, guelph,      3304.26).
edge(burlington,coquitlam,   4304.81).
edge(ajax, abbotsford,      3320.8 ).
edge(cambridge, victoria,   3333.53 ).
edge(gatineau, saskatoon,   2363.13 ).
edge(saintJohn, hamilton,   1120.76).
edge(terrebonne, toronto,   507.8).


% solutions
% used to get knowledge on graps; https://saksagan.ceng.metu.edu.tr/courses/ceng242/documents/prolog/jrfisher/2_15.html

%i connected(A,B).
connected(X, Y):- connected(X,Y,[]).

%base case
connected(X, Y, _):- edge(X,Y,_).

connected(X, Y, Lis):- not(member(X, Lis)),
                      edge(X, Z,_),
                      connected(Z, Y, [X|Lis]).

/*-----------------------------------------------------------*/

%ii commuterPath(A,B,L).
commuterPath(A,B,Path):- connected(A,B), getpath(A,B,Path,[A]).

%base case
getpath(A,B,Path,Lis):- edge(A,B,_), reverse([B|Lis],Path).

getpath(A,B,Path,Lis):- edge(A,X,_), not(member(X, Lis)), not(X = B),
                                 getpath(X, B, Path, [X|Lis]).

/*-----------------------------------------------------------*/

%iii pathLength(A,B,L)
pathLength(A,B,Len):- commuterPath(A,B,Path),getLength(Path,0,Len).

%base case
getLength(Lis,Total,Sum):- length(Lis,1), Sum is Total.

getLength([A,B|T],X,L):- edge(A,B,Length), X1 is Length+X, getLength([B|T],X1 ,L).
/*-----------------------------------------------------------*/

%iv getCycle(A,C)
getCycle(A,Cycle):- connected(A,A), commuterPath(A,A,Cycle).

/*-----------------------------------------------------------*/
%v longestPath(A,L).

longestPath(A,Path):- commuterPath(A,_,Path),getLength(Path,0,Len),
                      not((commuterPath(A,_,Path2),getLength(Path2,0,Len2), Len < Len2)).
