from mpi4py import MPI
comm = MPI.COMM_WORLD
rank = comm.rank
size = comm.size

#def formula():
#    print "loop"

if rank == 0:
    def formula():
        print "loopy"    

    
    shared1 = {'d1':55,'d2':42,'d6':7,'d9':formula()}
    comm.send(shared1,dest=1,tag=3) #here one can tag
    
    shared2 = {'d3':32,'d4':33}
    comm.send(shared2,dest=1,tag =2)
    
if rank == 1:
    receive1 = comm.recv(source=0, tag=3) #I want to see dic nr2
    #receive2 = comm.recv(source=0)
    #receive=  comm.recv(source=0,tag=1)
    print receive1
    formula()
    #print receive2
