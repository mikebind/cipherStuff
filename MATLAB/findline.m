function linenums = findline(str,nsnp)
k = findstr(str,lower(nsnp));
% k might be a vector
linenums = ceil(k./18);

