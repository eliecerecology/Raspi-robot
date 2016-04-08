from mpi4py import MPI
comm = MPI.COMM_WORLD
rank = comm.Get_rank()
size = comm.Get_size()

if rank == 0:
   data = [ 1,2,3,4,5,6]
   #print "we will be scattering:", data

else:
   data = None

data = comm.bcast(data, root= 0)
print 'rank', rank, 'has data:',data
