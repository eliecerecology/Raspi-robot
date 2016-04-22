data = xlsread('C:/Users/localadmin_eliediaz/Documents/MEGA/Storage/Mathlab/test c-sv data.xls');

%permutation_number = 1000;

size_data = size(data);  
sample_number = size_data(1);

sem = zeros(1,sample_number/2);

for lag = 1 : sample_number/2
    for i=1:sample_number-lag
        sum = (data(i+lag,1)-data(i,1))*(data(i+lag,2)-data(i,2));
        sem(lag) = sem(lag) + sum;
    end
    sem(lag) = sem(lag)/(2*(sample_number-lag));
end %cross-semivariogram natural

pmore = zeros(1,length(sem)); %create a vector of zeros (vector)
pless = zeros(1,length(sem));

for permutations = 1:1000
    rand_index = randperm(sample_number);
    data(:,1) = data(rand_index,1); %permute the variable 1
    rand_index = randperm(sample_number);
    data(:,2) = data(rand_index,2); %permute the variable 2
    
    permsem = zeros(1,sample_number/2); %generate a vector of zeros 
    
    % determine the semivariogram for permuted data
 for lag = 1 : sample_number/2
    for i=1:sample_number-lag
        sum = (data(i+lag,1)-data(i,1))*(data(i+lag,2)-data(i,2));
        permsem(lag) = permsem(lag) + sum; % It is jus appending cross-semivariogram permutation
    end
        permsem(lag) = permsem(lag)/(2*(sample_number-lag)); %cross-semivariogram permutation
  end
    pmore = pmore + (sem > permsem); % pmore is zero, add 1 (true), otherwise add 0
    pless = pless + (sem < permsem); 
end
