function oritext = orient(linessure,nsnpmat)
% Take lines in linessure, orient them, and output oriented matrix

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
oritext = blanks(18)
for i = 1:numgrps
    %Figure out shift for that group
    grplines = grp(i,1):(grp(i,1)+grp(i,2)-1);
    origtext = (nsnpmat(grplines,:))';
    origtextline = origtext(:)';
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
        dist = dist+18;
    end
    while dist>17
        dist = dist-18;
    end
    % dist should now be a positive number between 0 and 18
    oritextline =[blanks(dist) origtextline blanks(18-dist)]';
    oritextadd = reshape(oritextline,18,length(oritextline)/18)';
    oritext = [oritext;oritextadd];
end
%eliminate extraneous blanks
oritext(1,:) = [];

        