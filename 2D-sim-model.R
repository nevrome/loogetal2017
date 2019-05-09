###########################
### CONSTANT PARAMETERS ###
###########################

# world parameters
x_min <- -4000
x_max <- 4000
y_min <- -4000
y_max <- 4000

# population parameters
generations <- 301 # number of steps in random walk
in_entities <- 1 # initial number of entities
max_entities <- 10000	# carrying capacity - maximum number of entities the modelled domain can sustain

# craniometric (CM) measurement parameters
num_CM_meas <- 50 # total number of CM measurements considered

# gaussian distribution parameters - for CM measurements
CM_mu <- 1000 # mean
CM_sigma_frac <- 0.05 # standard deviation of CM gaussian - proportion of mean (CM_mu): 0.005, 0.05, 0.5
CM_sigma_val <- CM_mu*CM_sigma_frac # standard deviation of CM gaussian - value (CM_mu*CM_sigma_frac)

prob_fis <- 0.1 # probability with which each entity fissions at each generation
prob_dep <- 0.00001 # probability with which each entity is sampled (data recorded to output)


###########################
### VARIABLE PARAMETERS ###
###########################

# gaussian distribution parameters - for migration
mig_mu <- 0 # mean
#	mig_sigma = random.randint(1,125)	# 2.5 #					# standard deviation: 2.5, 5, 7.5, 10
mig_sigma <- runif(1,0,100)


##################
### INITIALISE ###
##################

# initial (empty) array for storing population density values in
entities <- in_entities

# initial positions
x <- matrix(runif(entities, x_min, x_max), entities, 1)
y <- matrix(runif(entities, y_min, y_max), entities, 1)

x_original <- x
y_original <- y

# initial CM measurements
CM_matrix <- matrix(CM_mu, entities, num_CM_meas)
CM_sigma <- matrix(CM_sigma_val, num_CM_meas, 1)


###########################
### START OF SIMULATION ###
###########################

for (n in 0:generations) {
  
  print(paste('generation: ', n, '  ', 'number of entities: ', entities))

  ### walk iteration ###
  xmove <- matrix(rnorm(entities, mig_mu, mig_sigma), entities, 1)	# amount to move by in x direction
  ymove <- matrix(rnorm(entities, mig_mu, mig_sigma), entities, 1)	# amount to move by in y direction
  
  # calculate new positions and check viability (geographical)
  xnew <- x + xmove	# proposed x position
  xpos <- ifelse(xnew < x_min | xnew > x_max, x, xnew) # check geographical viability of proposed x position
  x <- xpos # new x position
  
  ynew <- y + ymove # proposed  y position
  ypos <- ifelse(ynew < y_min | ynew > y_max, y, ynew) # check geographical viability of proposed y position
  y <- ypos	# new y position
  
  
  ### fission / extinction iteration ###
  # entities undergo fission with probability == prob_fis
  CM_matrix_original <- CM_matrix
  fis <- runif(entities) # entities undergo fission if value in corresponding fis array <= prob_fis

  # FISSION PROCESS - duplicate data for entities that undergo fission in x, y and CM_matrix
  fis_entities_ind <- which(fis <= prob_fis) # indices of entities undergoing fission

  if (length(fis_entities_ind) > 0) {
    x <- rbind(x, x[fis_entities_ind, , drop = FALSE])
    y <- rbind(y, y[fis_entities_ind, , drop = FALSE])
    CM_matrix <- rbind(CM_matrix, CM_matrix_original[fis_entities_ind, , drop = FALSE])
  }
  
  entities <- nrow(x)
  
  # check if number of entities has reached max_entities; if so:
  # EXTINCTION PROCESS - delete data for entities that undergo extinction from x, y and CM_matrix
  #	if entities>max_entities: print 'THERE ARE MORE ENTITIES THAN MAX ENTITIES - AN EXTINCTION PROCESS SHOULD BE HAPPENING HERE!'
  CM_matrix_original <- CM_matrix
  
  if (entities > max_entities) {	
    ext_entities_ind <- sample(0:entities, ifelse(entities >= max_entities, abs(entities - max_entities), 0)) # indices of surplus # of entities selected (random sampling) to undergo extinction

    if (length(ext_entities_ind) > 0) {
      x <- x[-ext_entities_ind, , drop = F]
      y <- y[-ext_entities_ind, , drop = F]
      CM_matrix = CM_matrix_original[-ext_entities_ind, , drop = F]
      entities = nrow(x)
    }
    
  }
  
  # check if all entities extinct
  if (entities == 0) {
    print(paste("All entities have gone extinct at iteration", n))
    break
  }
  

  ### mutation iteration ###
  # NOTE! each CM measurement of each entity mutates (varies slightly) between generations
  # CM measurements vary according to gaussian distribution with mean CM_matrix and s.d. CM_sigma
  CM_matrix <- matrix(
    data = rnorm(entities * num_CM_meas, as.vector(CM_matrix), as.vector(CM_sigma)), 
    entities, 
    num_CM_meas
  )
  CM_matrix <- abs(CM_matrix)
  
  
  ### feature depositing iteration ###
  # entities sampled with probability == prob_dep
  # for sampled entities, data recoded to output: 25*n, x, y, CM_matrix values
  if (n > generations/2) {
    dep <- rbinom(entities, 1, prob_dep)
    dep_entities_ind <- which(dep == 1) # indices of entities sampled
    if (length(dep_entities_ind) > 0) {
      dep_matrix <- data.frame(
        n25 = matrix(25*n, length(dep_entities_ind), 1), 
        x = x[dep_entities_ind,], 
        y = y[dep_entities_ind,], 
        CM_matrix[dep_entities_ind, , drop = F]
      )		# 25*n (25 is the generation length in years?) , x, y and CM_matrix values for entities sampled
      if (!exists("final_output")) {
        final_output <- dep_matrix
      } else {
        final_output <- rbind(final_output, dep_matrix)
      }
    }
  }
  
}
