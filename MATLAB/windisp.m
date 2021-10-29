bot = 35;
close all

% multiline edit box for original and translation
fig1 = figure('NumberTitle','off','position',[1,bot,800,600],'Name', 'Original Text with Translation');
wholeedit = uicontrol(fig1,'Style','Edit','Position',[5,5,790,590],'Max',2);

% another for oriented text and translation
%fig2 = figure('NumberTitle','off','Position',[400,bot,400,600],'Name', 'Oriented Pieces with Translation');
%oriedit = uicontrol(fig2,'Style','Edit','Position',[5,5,390,590],'Max',2);

% another for columned text and translation 
fig3 = figure('NumberTitle','off','Position',[800,bot,200,600],'Name', 'Oriented Pieces in column form');
coledit = uicontrol(fig3,'Style','Edit','Position',[5,5,190,590],'Max',2);

set([wholeedit,coledit],'FontName','FixedWidth','HorizontalAlignment','Left');
set([fig1,fig2,fig3],'Menubar','none')

% a place for entering substitutions and 
%listing where replacements are <- Command window

% use bsm and subfuncwin (based on subfunc2)

% show substitution matrix and histogram for oriented text?

% be able to add lines to oriented text
