module linkedlistmod_priv
  implicit none

  type linkedlist_t
     ! TODO: A list contains a private pointer to the first list element
  end type linkedlist_t

  type, private :: listelement_t
     ! TODO: A listelement contains private key, value and a private pointer
     ! to the next list element
  end type listelement_t

  type listiterator_t
     private
     type(listelement_t), pointer :: next => null()
  end type listiterator_t

  contains 

    subroutine insert(list, key, value)
      implicit none
      type(linkedlist_t) :: list
      integer, intent(in) :: key, value
      
      type(listelement_t), pointer :: elem
      integer :: allocstat

      ! Allocate new list element
      allocate(elem, STAT=allocstat)
      if (allocstat /= 0) then
         write (*,*) 'insert: Memory allocation error'
      end if
      elem % key = key
      elem % value = value

      ! Add element as the first one in list
      elem % next => list % first
      list % first => elem
      list % numberofelements = list % numberofelements + 1
    end subroutine insert

    subroutine deletefirst(list)
      implicit none
      type(linkedlist_t) :: list
      type(listelement_t), pointer :: elem
      
      if (associated(list % first)) then
         elem => list % first
         list % first => list % first % next
         deallocate(elem)
         list % numberofelements = list % numberofelements - 1
      end if
    end subroutine deletefirst

    subroutine delete(list, key) 
      implicit none
      type(linkedlist_t) :: list
      integer, intent(in) :: key

      type(listelement_t), pointer :: elem, relem

      ! First element
      elem => list % first
      if (associated(elem)) then
         if (elem % key == key) then
            list % first => elem % next
            deallocate(elem)
            list % numberofelements = list % numberofelements - 1
            return
         end if
      end if

      ! Find element by traversing the list
      do while (associated(elem % next))
         if (elem % next % key == key) then
            relem => elem % next
            elem % next => elem % next % next
            deallocate(relem)
            list % numberofelements = list % numberofelements - 1
            exit
         end if
         elem => elem % next
      end do
    end subroutine delete

    function search(list, key, value) result(found)
      implicit none
      type(linkedlist_t) :: list
      integer, intent(in) :: key
      integer, intent(out) :: value
      logical :: found
      
      type(listelement_t), pointer :: elem
      
      elem => list % first
      value = 0
      found = .false.
      do while (associated(elem))
         if (elem % key == key) then
            value = elem % value
            found = .true.
            exit
         end if
         
         elem => elem % next
      end do
    end function search

    function getsize(list) result(listsize)
      implicit none
      type(linkedlist_t) :: list
      integer :: listsize

      listsize = list % numberofelements
    end function getsize

    function getiterator(list) result(iter)
      implicit none
      type(linkedlist_t) :: list
      type(listiterator_t) :: iter

      iter % next => list % first
    end function getiterator

    function next(iter) result(nextelem)
      implicit none
      type(listiterator_t) :: iter
      integer :: nextelem

      nextelem = iter % next % value
      iter % next => iter % next % next
    end function next

    function nextKey(iter) result(key)
      implicit none
      type(listiterator_t) :: iter
      integer :: key
      
      key = iter % next % key
    end function nextKey

    function hasnext(iter)
      implicit none
      type(listiterator_t) :: iter
      logical :: hasnext

      hasnext = associated(iter % next)
    end function hasnext

end module linkedlistmod_priv
