      module modevent
!!    ~ ~ ~ Author ~ ~ ~
!!    Andre Dozier, PhD student
!!    Colorado State University 2012-2016
!!    Comment initials "aqd"
!!
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    Implements a generic code event that can be subscribed to by any number of subscribers with two different structures: 
!!     - a subroutine with the following format can be subscribed to an event by "call my_event%subscribe(my_subscriber)"
!!          subroutine my_subscriber(eventdata)
!!              class (ieventdata), pointer :: eventdata
!!              <my_work>      
!!          end subroutine 
!!
!!     - a subclass with the following format can be subscribed to an event by "call my_event%subscribe_object(my_subscriber_subroutine)"
!!          type, public, extends(isubscriber) :: my_subscriber_class
!!              <my_data>               
!!          contains
!!              procedure, pass :: run => my_subscriber_subroutine
!!          end type
!!
!!          subroutine my_subscriber_subroutine(subsc, eventdata)
!!              class (my_subscriber_class), intent(inout) :: subsc
!!              class (ieventdata), pointer :: eventdata
!!              <my_work>
!!          end subroutine
!!
!!    An event can be instantiated by "type(event), target :: my_event"
!!    An event can be "fired" by "call my_event%fire()".
!!       
      implicit none

        ! Parameters
        logical :: verbose = .false.

        ! Define a generic subscriber 
        type, public, abstract :: isubscriber
            integer :: ind = -1
            class (isubscriber), pointer :: next => null() 
        contains 
            procedure(abstrun), deferred, pass :: run
        end type 

        ! Define the interface for event data
        type, public, abstract :: ieventdata
        end type

        ! Define abstrun 
        abstract interface 
            subroutine abstrun(subsc, eventdata)
                import :: isubscriber
                import :: ieventdata
                class (isubscriber), intent(inout) :: subsc
                class (ieventdata), pointer :: eventdata
            end subroutine 
        end interface 

        ! Running a simplified subscriber's subroutine 
        interface 
            subroutine simple(eventdata)
                import :: ieventdata
                class (ieventdata), pointer :: eventdata
            end subroutine simple 
        end interface 

        ! Define the simplified subscriber
        type, public, extends(isubscriber) :: subscriber
            procedure (simple), pointer, nopass :: simple => null()
        contains 
            procedure, pass :: run => simplerun
        end type subscriber

        ! Define the event class
        type, public :: event
            integer :: subindex = 0
            class (ieventdata), pointer :: data => null()
            class (isubscriber), pointer :: first => null()
            class (isubscriber), pointer :: last => null()  
        contains
            procedure :: subscribe
            procedure :: subscribe_object
            procedure :: unsubscribe
            procedure :: fire
        end type event

      contains

        ! Run the subscriber's event 
        subroutine simplerun(subsc, eventdata)
            class (subscriber), intent(inout) :: subsc
            class (ieventdata), pointer :: eventdata

            if (associated(subsc%simple)) call subsc%simple(eventdata)
        end subroutine 

        ! Subscribe to the event
        function subscribe(self, newsubscriber)
            integer :: subscribe
            class(event), intent(inout) :: self
            type (subscriber), pointer :: subsc
            class(isubscriber), pointer :: isubsc

            interface
                subroutine newsubscriber(eventdata)
                    import :: ieventdata
                    class (ieventdata), pointer :: eventdata
                end subroutine newsubscriber
            end interface

            ! set the subscriber's run method to the new method
            allocate(subsc)
            subsc%simple => newsubscriber

            ! allocate the subscriber pointer
            allocate(isubsc, source=subsc)

            ! subscribe the object
            call self%subscribe_object(isubsc)

            ! return the index of the subscriber (for unsubscription later)
            subscribe = isubsc%ind
        end function

        ! Subscribes a subscriber object 
        subroutine subscribe_object(self, newsubscriber)
            class(event), intent(inout) :: self
            class(isubscriber), pointer :: newsubscriber

            ! assign the subscribers index 
            newsubscriber%ind = self%subindex
            if (verbose) print *,'adding index ',self%subindex

            ! ensure the first subscriber is allocated 
            if (.not.associated(self%first)) then 
                self%first => newsubscriber
                self%last => newsubscriber
            else 
                self%last%next => newsubscriber 
                self%last => self%last%next
            end if 

            ! increment the index of the subscriber
            self%subindex = self%subindex + 1
        end subroutine

        ! Unsubscribe to the event
        subroutine unsubscribe(self, unsubi)
            class(event), intent(inout) :: self
            integer :: unsubi
            class (isubscriber), pointer :: prev, sub

            ! exit if there are no subscribers
            if (.not.associated(self%first)) return 

            ! find the subscriber with the same index
            prev => null()
            sub => self%first
            if (verbose) print *,'looking for index ',unsubi
            do while (sub%ind /= unsubi)
                if (.not.associated(sub%next)) return
                prev => sub
                sub => sub%next
            end do

            ! remove the subscriber and deallocate it
            if (associated(prev)) then 
                ! remove sub
                if (associated(sub%next)) then 
                    ! point to the next subscriber
                    prev%next => sub%next
                else
                    ! point to the second to last subscriber
                    if (verbose) print *,'removing end subscriber'
                    deallocate(prev%next)
                    self%last => prev
                end if 
                deallocate(prev)
                deallocate(sub) 
            else 
                ! delete the first subscriber
                if (associated(sub%next)) then
                    ! point to the second subscriber if existent
                    if (verbose) print *,'removing first subscriber'
                    self%first => sub%next
                    deallocate(sub) 
                else
                    ! the first subscriber is the only one left
                    if (verbose) print *,'removing all subscribers'
                    deallocate(self%first) 
                end if 
            end if 
        end subroutine

        ! Fire the event
        subroutine fire(self)
            class(event), intent(in) :: self
            integer :: i
            class (isubscriber), pointer :: sub, nextsub
            
            ! exit if there are no subscribers
            if (.not.associated(self%first)) return 

            ! run all the subscribers 
            sub => self%first
            do  ! while (associated(sub%next)) 
                if (associated(sub%next)) nextsub => sub%next
                call sub%run(self%data)
                if (.not.associated(sub%next)) return
                sub => nextsub
            end do
            deallocate(sub)
        end subroutine

      end module modevent

