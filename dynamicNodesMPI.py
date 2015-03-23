from mpi4py import MPI
comm = MPI.COMM_WORLD
rank = comm.rank
size = comm.size
name = MPI.Get_processor_name()

shared = (rank+1)*7 #data

comm.send(shared, dest=(rank+1)%size)
data = comm.recv(source=(rank-1)‰size)

print nanme
print "rank", rank
print "recieved:", data, "came from rank", (rank-1)%size