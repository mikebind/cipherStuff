function subtext = subfunc2(oritext,subsmat)
% subsmat should be an 18x26 matrix which is organized by 
% cipher alphabet.  The first row corresponds to the first alphabet
% and a 'P' in the 4th column means that in the first cipher alphabet
% the ciphertext letter 'd' (the fourth letter) corresponds to the 
% plaintext letter 'p'

alphabet = ['abcdefghijklmnopqrstuvwxyz'];

[alphnum,col] = find(subsmat~=blanks(1));
for i = 1:length(alphnum)
    replacement(i) = subsmat(alphnum(i),col(i));
end
subtext = oritext;
for i = 1:length(alphnum)
    subtext = subfunc1(subtext,alphnum(i),alphabet(col(i)),replacement(i));
end
  
return
