function [oricol, lettcol] = orient(linessure,nsnpmat,lettindmat,n)
% Take lines in linessure, orient them, and output oriented matrix
% n is number of columns

% Identify grouped lines
diffs = linessure(2:end)-linessure(1:end-1);
grpbrks = find(diffs~=1);
grpbrks = [1,grpbrks];
numgrps = length(grpbrks);
for i = 1:numgrps
    if i ==1 
        grp(i,1) = linessure(grpbrks(i));
        grp(i,2) =grpbrks(2);
    elseif i==numgrps
        grp(i,1) = linessure(grpbrks(i)+1);
        grp(i,2) = length(linessure)-grpbrks(i);
    else
        grp(i,1) = linessure(grpbrks(i)+1);
        grp(i,2)= grpbrks(i+1)-grpbrks(i);
    end
end

%Orient groups
ncgpos = 5;
ixbpos = 10;
ixppos = 10;
wzfzpos = 6;
oricol = blanks(n);
lettcol = zeros(1,n);

for i = 1:numgrps
    %Figure out shift for that group
    grplines = grp(i,1):(grp(i,1)+grp(i,2)-1);
    origtext = (nsnpmat(grplines,:))';
    origlett = (lettindmat(grplines,:))';
    origtextline = origtext(:)';
    origlettline = origlett(:)';
    %Check for ixb,ixp,ncg
    k = findstr(origtextline,'ixb');
    ksb = ixbpos;
    if isempty(k)
        k = findstr(origtextline,'ixp');
        ksb = ixppos;
        if isempty(k)
            k = findstr(origtextline,'ncg');
            ksb = ncgpos;
            if isempty(k)
                k= findstr(origtextline,'wzfz');
                ksb =wzfzpos;
                if isempty(k)
                    disp('Group does not contain ixp,ixb,or ncg')
                    keyboard
                end
            end
        end
    end
    % Now to make k into ksb
    dist = ksb-k(1);
    while dist<0
        dist = dist+n;
    end
    while dist>n-1
        dist = dist-n;
    end
    % dist should now be a positive number between 0 and 18
    oricolline =[blanks(dist) origtextline blanks(n-dist)]';
    lettcolline = [zeros(1,dist) origlettline zeros(1,n-dist)]';
    oricoladd = reshape(oricolline,n,length(oricolline)/n)';
    lettcoladd = reshape(lettcolline,n,length(lettcolline)/n)';
    oricol = [oricol;oricoladd];
    lettcol = [lettcol;lettcoladd];
    
end
%eliminate extraneous blanks
oricol(1,:) = [];
lettcol(1,:) = [];

        