%%% Matlab m-file to perform counterfactuals for "What Goods Do Countries Trade? A Quantitative Exploration of the Ricardian Model" 
%%% By Arnaud Costinot, Dave Donaldson and Ivana Komunjer
%%% Review of Economic Studies, 2011
  
clear all

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% CONSIDER DIFFERENT SCENARIOS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
% The code runs through 4 different scenarios (defined in Section 5 of the paper) as follows:
    % Scenario 0: heterogeneous tastes (ALPHA(j,k) is expenditure share by country), true trade data(X = X).
    % Scenario 1: homogeneous tastes (ALPHA(j,k) is expenditure share for world), true trade data(X = X).
    % Scenario 2: heterogeneous tastes (ALPHA(j,k) is expenditure share by country), predicted trade data.
    % Scenario 3: homogeneous tastes (ALPHA(j,k) is expenditure share of world), predicted trade data.

    
for scenario = 0:3
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% SET EXOGENOUS PARAMETERS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    global theta N K ALPHA Gamma PI ref z 

    % size of economy:
        K = 13;
        N = 21;

    % 'theta', estimated in 'ricardo_regs.do'    
        theta = 6.53;

    % input the X matrix (trade data) from csv files:
        %X: first index is i (exporter), scond is j (importer), third is k (industries) 
        X_data = ones(N,N,K);
        for k = 1:K
            kstr = num2str(k);
            if scenario <2
                X_data(:,:,k) = double(csvread(['X' kstr '.csv'],0,0,[0 0 N-1 N-1]));
            else 
                X_data(:,:,k) = double(csvread(['X' kstr '_3FE_pred.csv'],0,0));
            end
        end

    % create other variables from the trade data:    
        Y_data = ones(N,1);
        for i = 1:N
            Y_data(i) = sum(sum(X_data(i,:,:),3),2);
        end

        Gamma_data = ones(N,1);
        for i = 1:N
            Gamma_data(i) = Y_data(i)/sum(Y_data,1);
        end
        ALPHA_data = ones(N,K);        
        for j = 1:N
            for k = 1:K
            ALPHA_data(j,k) = sum(X_data(:,j,k),1)/(sum (sum(X_data(:,j,:),3),1));
            end
        end

        if scenario == 1 || scenario==3 
            alpha_data=zeros(1,N);
            for k = 1:K
                alpha_data(k) = sum( sum(X_data(:,:,k),2),1)/sum(Y_data,1);
            end
            ALPHA_data = (alpha_data'*ones(1,N))';
        end

        PI_data = ones(N,N,K);
        for i = 1:N
            for j = 1:N
                for k = 1:K
                PI_data(i,j,k) = X_data(i,j,k)/sum(X_data(:,j,k),1);
                end
            end
        end


    %input the productivity data:
        FE_data = double(csvread('rescale_FE_matrix.csv',0,0,[0 0 N-1 K-1]));
        z_data = exp(FE_data./theta);


    %%%%%%%%%%%%%%%%%%%% SOLVE THE COUNTERFACTUAL WELFARE CHANGE FOR EACH REFERENCE COUNTRY %%%%%%%%%%%%%%%%%%    
   

    % loop over the countries, with each being the reference country (ie 'ref') one at a time.
    exitflagvector = zeros(1,N);
    logW_change_vector = zeros(1,N);
    logW_change_A_vector = zeros(1,N);
    logX_change_total_vector = zeros(1,N);
    GL_change_vector = zeros(1,N);
    GL_data_vector = zeros(1,N);
    
    for c = 1:N
         ref=c;

    % Compute the counterfactual Z:
        PI = PI_data;
        Gamma = Gamma_data;
        ALPHA = ALPHA_data;
        z = z_data;

        options=optimset('MaxIter', 1000,...
                'TolX',1e-25,...
                'TolFun',1e-25);
          %'Display','iter',...  

        lb = zeros(N,1);  
        ub = Inf*ones(N,1);
        Z_prime0 = (0.5+rand(N,1));
        Z_prime0 = Z_prime0./Z_prime0(ref);
        [Z_prime, resnorm, residual,exitflag] = lsqnonlin(@Zfunction,Z_prime0,lb,ub,options);
        exitflagvector(c) = exitflag;

    % Compute counterfactual welfare change:
        isumterm = ones(N,K);
        for i = 1:N
           for k = 1:K
               isumterm(i,k) = Z_prime(i).*( (z_data(i,k)/z_data(ref,k))^(-theta) ).*PI_data(i,ref,k);
           end
        end
        isum = ones(K,1);
        for k = 1:K
            isum(k) = sum(isumterm(:,k),1);
        end

        logW_change_sumterm = ones(K,1);
        for k = 1:K
            logW_change_sumterm(k) = ALPHA(ref,k).*log(isum(k));
        end

        logW_change = (1/theta)*sum(logW_change_sumterm,1);
        logW_change_vector(c) = logW_change;

        logW_changeA_sumterm = ones(K,1);
        for k = 1:K
            logW_changeA_sumterm(k) = ALPHA(ref,k).*log(PI_data(ref,ref,k));
        end
        logW_change_A = (1/theta)*sum(logW_changeA_sumterm,1);
        logW_change_A_vector(c) = logW_change_A;

    % compute counterfactual trade flow change:
        zZ = ones(N,K);
        for i = 1:N
           for k = 1:K
        zZ(i,k) = Z_prime(i)*(z_data(i,k).^(-theta));
           end
        end

        X_change = ones(N,N,K);
        for i = 1:N
           for j = 1:N
               for k = 1:K
                    X_change(i,j,k) = zZ(i,k)/(zZ(:,k)'*PI_data(:,j,k));
               end
           end
        end

        X_new = X_data.*X_change;

        Ex_data_totalk(ref,:) = sum(X_data(ref,:,:),2)-X_data(ref,ref,:);
        Im_data_totalk(ref,:) = sum(X_data(:,ref,:),1) - X_data(ref,ref,:);
        Ex_new_totalk(ref,:) = sum(X_new(ref,:,:),2) - X_new(ref,ref,:);
        Im_new_totalk(ref,:) = sum(X_new(:,ref,:),1) - X_new(ref,ref,:);
        X_data_totalk(ref,:) = sum(X_data(ref,:,:),2);
        X_new_totalk(ref,:) = sum(X_new(ref,:,:),2);


        Ex_change_total = sum(Ex_new_totalk(ref,:),2)./sum(Ex_data_totalk(ref,:),2);
        X_change_total = sum(X_new_totalk(ref,:),2)./sum(X_data_totalk(ref,:),2);
        logX_change_total_vector(c) = 100*log(X_change_total);

        GL_data_num(ref,:) = abs(Ex_data_totalk(ref,:) - Im_data_totalk(ref,:) );
        GL_data_denom(ref,:) = Ex_data_totalk(ref,:)+Im_data_totalk(ref,:);
        GL_data = 100*(sum(GL_data_num(ref,:),2)/sum(GL_data_denom(ref,:),2));

        GL_new_num(ref,:) = abs(Ex_new_totalk(ref,:) - Im_new_totalk(ref,:) );
        GL_new_denom(ref,:) = Ex_new_totalk(ref,:)+Im_new_totalk(ref,:);
        GL_new = 100*(sum(GL_new_num(ref,:),2)/sum(GL_new_denom(ref,:),2));

        GL_change = GL_new - GL_data;
        GL_change_vector(c) = GL_change;
        GL_data_vector(c) = GL_data;

    end % loop over c

    logW_change_vector(exitflagvector==0)=NaN;
    logW_change_vector = 100.*logW_change_vector;
    logW_change_reltoA_vector = (logW_change_vector./logW_change_A_vector);

    logEx_change_total_vector(exitflagvector==0)=NaN;
    logX_change_total_vector(exitflagvector==0)=NaN;

    GL_change_vector(exitflagvector==0)=NaN;
    GL_data_vector(exitflagvector==0)=NaN;
    GL_data_mean = mean(GL_data_vector,1);

    table7output = [logX_change_total_vector' GL_change_vector' logW_change_vector' -logW_change_reltoA_vector'];
    table7output=[table7output;mean(table7output,1)];
    if scenario==0
        xlswrite(['counterfactuals_table7.xls'],table7output);
    end

    table8output(scenario+1,:) = table7output(N+1,:);

end % loop over scenarios

xlswrite(['counterfactuals_table8.xls'],table8output);

