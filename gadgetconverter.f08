program gadgetconverter

   use shared_module_core
   use shared_module_arguments
   use shared_module_hdf5

   implicit none
   
   character(len=255)   :: fin
   character(len=255)   :: fout
   character(len=11)    :: dataset
   integer*4            :: npart(6),ntot,n,i,n_elements,empty(58)
   real*4,allocatable   :: x(:,:),v(:,:)
   integer*4,allocatable:: id(:)
   
   ! start user interface
   call set_version('0.2')
   call handle_arguments(require_task=.false.)
   call start_output
   
   ! handle options
   call get_option_value(fin,'-in')
   call get_option_value(fout,'-out')
   call require_no_options_left
   
   ! load file
   call out('Read file: ',trim(fin))
   npart = 0
   call hdf5_open(fin)
   
   do i = 1,6
      write(dataset,'(A,I1,A)') '/PartType',i-1,'/'
      if (hdf5_object_exists(dataset)) then
         call hdf5_get_dataset_size(dataset//'ParticleIDs', n_elements=n_elements)
         npart(i) = n_elements
      end if
   end do
   
   ntot = sum(npart)
   call out('Total numer of particles: ',ntot)
   
   allocate(x(3,ntot))
   allocate(v(3,ntot))
   allocate(id(ntot))
   n = 0
   do i = 1,6
      write(dataset,'(A,I1,A)') '/PartType',i-1,'/'
      if (npart(i)>0) then
         call hdf5_read_data(dataset//'Coordinates', x(:,(n+1):(n+npart(i))))
         call hdf5_read_data(dataset//'Velocities', v(:,(n+1):(n+npart(i))))
         call hdf5_read_data(dataset//'ParticleIDs', id((n+1):(n+npart(i))))
         n = n+npart(i)
      end if
   end do
   call hdf5_close()
   
   ! save file
   empty = 0
   call out('Save file: '//trim(fout))
   open(1,file=trim(fout),action='write',form='unformatted',status='replace',access='stream')
   write(1) 256,npart,empty,256
   write(1) 3*n,x,3*n
   write(1) 3*n,v,3*n
   write(1) n,id,n
   close(1)
   
   ! finalize output on screen/logfile
   call stop_output
    
end program gadgetconverter