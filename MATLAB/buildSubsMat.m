function subsMat = buildSubsMat(wsubT,frame)

% initialize
subsMat = repmat('_',18,26);
% Build
for sub_idx = 1:length(wsubT.nsnp_start)    
    nsnp_idx = wsubT.nsnp_start(sub_idx);
    plain = wsubT.plain{sub_idx};
    % Incorporate this sub into subsMat
    subsMat = updateSubsMat(nsnp_idx, plain, subsMat, frame);
end
    
    