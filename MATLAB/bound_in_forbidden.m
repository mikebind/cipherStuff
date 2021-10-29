function tf = bound_in_forbidden(step_boundaries, forbidden)

% Check if any of the step boundaries are in the forbidden zones
tf = false; 
for bnd = step_boundaries
    if any(bnd>forbidden(:,1) & bnd<=forbidden(:,2))
        tf = true;
        return
    end
end