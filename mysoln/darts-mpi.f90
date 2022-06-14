program darts_mpi
   
   ! Add module use statements
   ! use mpi
   use lcgenerator
   
   implicit none

   include 'mpif.h'
   
   integer*8 :: num_trials = 1000000, i = 0, Ncirc = 0
   real :: pi = 0.0, x = 0.0, y = 0.0, r = 1.0
   real :: r2 = 0.0
   real :: lcgrandom
   
   ! Add mpi variables
   integer :: rank, size, error, p, status
   integer :: root = 0
   real    :: my_num_trials
   integer*8 :: Ncirc_temp
   
   ! initialize mpi
   call MPI_Init(error)
   call MPI_Comm_size(MPI_COMM_WORLD, size, error)
   call MPI_Comm_rank(MPI_COMM_WORLD, rank, error)
   
   ! Set the number of trials for this processor
   my_num_trials = num_trials / size
   if (rank .lt. mod(num_trials,size)) my_num_trials = my_num_trials + 1
   call seed(rank)
   
   ! modify the loop to only iterate through my_num_trials for the current processor
   r2 = r*r
   do i = 1, my_num_trials
      x = lcgrandom()
      y = lcgrandom()
      if ((x*x + y*y) .le. r2) then
         Ncirc = Ncirc+1
      end if
   end do

   ! Send the number in the circle to the master process, recieve from 
   if (rank .ne. root) then
      call MPI_send(Ncirc, 1, MPI_LONG, root, rank, MPI_COMM_WORLD)
   else
      do p = 1, size-1
         call MPI_recv(Ncirc_temp, 1, MPI_LONG, p, p, MPI_COMM_WORLD, status, error)
         Ncirc = Ncirc + Ncirc_temp
      enddo

      pi = 4.0*((1.0*Ncirc)/(1.0*num_trials))
      print*, '	'
      print*, '	Computing pi in serial:		'
      print*, ' 	For ', num_trials, ' trials, pi = ', pi
      print*, '	'
      
   endif

end program darts_mpi
