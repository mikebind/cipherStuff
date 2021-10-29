function [interwhole, transwhole, transcol] = subfuncwin2(oricol,lettcol,subsmat,wholetext,coledit,wholeedit)
% subsmat should be an 18x26 matrix which is organized by 
% cipher alphabet.  The first row corresponds to the first alphabet
% and a 'P' in the 4th column means that in the first cipher alphabet
% the ciphertext letter 'd' (the fourth letter) corresponds to the 
% plaintext letter 'p'

alphabet = ['abcdefghijklmnopqrstuvwxyz'];

%Set up outputs
transwhole = blanks(length(wholetext));
transcolline = blanks(size(oricol,1)*size(oricol,2));
transcol = reshape(transcolline,size(oricol,1),size(oricol,2));

[alphnum,col] = find(subsmat~=blanks(1));
for i = 1:length(alphnum)
    replacement(i) = subsmat(alphnum(i),col(i));
    %replacement is the translated letter
end

for i = 1:length(alphnum)
    letter =alphabet(col(i));
    sub = find(oricol(:,alphnum(i))==letter);
    transcol(sub,alphnum(i)) = replacement(i);
    transwhole(lettcol(sub,alphnum(i))) = replacement(i);
end

%add puncuation and numbers to transwhole
non_lett = ~isletter(wholetext);
transwhole(non_lett) = wholetext(non_lett);

%display transcol interlaced with oricol
intercol = interlace(transcol,oricol);
set(coledit,'String',intercol);

%display transwhole interlaced with wholetext
%this is harder
% need to set wrap on wholetext, make it into character array
wrap = 90;
remainder = rem(length(wholetext),wrap);
wholetextpad = [wholetext blanks(wrap-remainder)];
transwholepad = [transwhole blanks(wrap-remainder)];
wholetextwrap = reshape(wholetextpad,wrap,length(wholetextpad)/wrap)';
transwholewrap = reshape(transwholepad,wrap,length(wholetextpad)/wrap)';
interwhole = interlace(transwholewrap,wholetextwrap);

set(wholeedit,'String',interwhole)

return
