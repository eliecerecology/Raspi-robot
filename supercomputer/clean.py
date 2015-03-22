from mpi4py import MPI
comm = MPI.COMM_WORLD
rank = comm.rank
size = comm.size

if rank == 0:
    s=3   
    
    shared1 = {'d1':55,'d2':42,'d6':7,'d9':s*3}
    comm.send(shared1,dest=1,tag=3) #here one can tag
  
    share0 = s*3
    comm.send(share0,dest=1, tag=23)    
  
    shared2 = {'d3':32,'d4':33}
    comm.send(shared2,dest=1,tag =2)
    
if rank == 1:
    s=2
    receive1 = comm.recv(source=0, tag=23) #I want to see dic nr2
    receive0 = comm.recv(source=0,tag =3)
    
#receive2 = comm.recv(source=0)
#    print receive0
    print receive1
    print receive0
    print   range(size)
