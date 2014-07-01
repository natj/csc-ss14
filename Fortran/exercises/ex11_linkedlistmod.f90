module linkedlistmod
  implicit none

  type linkedlist_t
     ! TODO: A list contains a pointer to first element in list
     type(listelement_t), pointer :: first => null()
  end type linkedlist_t

  type listelement_t
     ! TODO: listelement contains key, value and a pointer 
     ! to the next element
  end type listelement_t

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
      elem % next => list % first

      ! Add new element as the first one
      list % first => elem
    end subroutine insert

    subroutine deletefirst(list)
      implicit none
      type(linkedlist_t) :: list
      type(listelement_t), pointer :: elem
      
      if (associated(list % first)) then
         elem => list % first
         list % first => list % first % next
         deallocate(elem)
      end if
    end subroutine deletefirst

    function isempty(list) result(empty)
      implicit none
      type(linkedlist_t) :: list
      logical :: empty
      
      empty = .not. associated(list % first)
    end function isempty
    
end module linkedlistmod
