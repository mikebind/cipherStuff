function [all_letts, diffs] = movhist(numlines,nsnpmat)
%movhist

alphabet = ['abcdefghijklmnopqrstuvwxyz'];

for i = 1:26
    eval([alphabet(i) 's = lower(nsnpmat)== alphabet(i);'])
end

% Make histogram
% numlines = 50;
[m, n] = size(nsnpmat);

for i = 1:m-numlines
    for j = 1:26
        eval([alphabet(j) 'hist(i,:) = sum(' alphabet(j) 's(i:i+numlines-1,:));'])
    end
end

for i = 1:26
    eval(['all_letts(:,:,i) = ' alphabet(i) 'hist;'])
end

for i = 1:(m-2*numlines)
    diffs(i) = sum(sum((all_letts(i,:,:)-all_letts(i+numlines,:,:)).^2));
end
figure
plot(diffs)


