document.addEventListener('DOMContentLoaded', function() {
    // Mobile menu toggle
    const burger = document.querySelector('.burger');
    const navLinks = document.querySelector('.nav-links');
    
    burger.addEventListener('click', () => {
        navLinks.classList.toggle('active');
        burger.classList.toggle('active');
    });
    
    // Smooth scrolling for navigation links
    document.querySelectorAll('nav a').forEach(anchor => {
        anchor.addEventListener('click', function(e) {
            if (this.getAttribute('href').startsWith('#')) {
                e.preventDefault();
                
                const targetId = this.getAttribute('href');
                const targetElement = document.querySelector(targetId);
                
                window.scrollTo({
                    top: targetElement.offsetTop - 80,
                    behavior: 'smooth'
                });
                
                // Close mobile menu if open
                if (navLinks.classList.contains('active')) {
                    navLinks.classList.remove('active');
                    burger.classList.remove('active');
                }
            }
        });
    });
    
    // Modal functionality
    const loginBtn = document.getElementById('loginBtn');
    const loginModal = document.getElementById('loginModal');
    const confirmationModal = document.getElementById('confirmationModal');
    const closeModals = document.querySelectorAll('.close-modal, #closeConfirmation');
    
    function openModal(modal) {
        modal.classList.add('show');
        document.body.style.overflow = 'hidden';
    }
    
    function closeModal(modal) {
        modal.classList.remove('show');
        document.body.style.overflow = '';
    }
    
    loginBtn.addEventListener('click', () => openModal(loginModal));
    
    closeModals.forEach(btn => {
        btn.addEventListener('click', function() {
            const modal = this.closest('.modal');
            closeModal(modal);
        });
    });
    
    window.addEventListener('click', (e) => {
        if (e.target.classList.contains('modal')) {
            closeModal(e.target);
        }
    });
    
    // Alert system
    function showAlert(message, type = 'success', duration = 5000) {
        const alertContainer = document.getElementById('alertContainer');
        const alertDiv = document.createElement('div');
        alertDiv.className = `alert alert-${type}`;
        alertDiv.textContent = message;
        
        alertContainer.appendChild(alertDiv);
        
        setTimeout(() => {
            alertDiv.classList.add('fade-out');
            setTimeout(() => alertDiv.remove(), 300);
        }, duration);
    }
    
    // Car data
    const cars = [
        {
            id: 1,
            make: 'Toyota',
            model: 'Camry',
            year: 2022,
            type: 'sedan',
            seats: 5,
            pricePerDay: 45,
            image: 'https://images.unsplash.com/photo-1618843479313-40f8afb4b4d8?ixlib=rb-1.2.1&auto=format&fit=crop&w=1350&q=80',
            features: ['Bluetooth', 'Backup Camera', 'Keyless Entry']
        },
        {
            id: 2,
            make: 'Honda',
            model: 'CR-V',
            year: 2021,
            type: 'suv',
            seats: 5,
            pricePerDay: 65,
            image: 'https://images.unsplash.com/photo-1568605117036-5fe5e7bab0b7?ixlib=rb-1.2.1&auto=format&fit=crop&w=1350&q=80',
            features: ['Apple CarPlay', 'AWD', 'Sunroof']
        },
        {
            id: 3,
            make: 'BMW',
            model: 'X5',
            year: 2023,
            type: 'luxury',
            seats: 5,
            pricePerDay: 120,
            image: 'https://images.unsplash.com/photo-1555215695-3004980ad54e?ixlib=rb-1.2.1&auto=format&fit=crop&w=1350&q=80',
            features: ['Leather Seats', 'Navigation', 'Premium Sound']
        },
        {
            id: 4,
            make: 'Ford',
            model: 'Mustang',
            year: 2022,
            type: 'sports',
            seats: 4,
            pricePerDay: 90,
            image: 'mustang-1.jpg',
            features: ['Convertible', 'Sport Mode', 'Premium Wheels']
        },
        {
            id: 5,
            make: 'Chevrolet',
            model: 'Tahoe',
            year: 2021,
            type: 'luxury',
            seats: 2,
            pricePerDay: 85,
            image: 'https://images.unsplash.com/photo-1553440569-bcc63803a83d?ixlib=rb-1.2.1&auto=format&fit=crop&w=1350&q=80',
            features: ['Third Row', 'Towing Package', 'DVD System']
        },
        {
            id: 6,
            make: 'Tesla',
            model: 'Model 3',
            year: 2023,
            type: 'sedan',
            seats: 5,
            pricePerDay: 65,
            image: '1081758-tesla.webp',
            features: ['Electric', 'Autopilot', 'Touchscreen']
        },
        {
            id: 7,
            make: 'HONDA',
            model: 'AMAZE',
            year: 2022,
            type: 'sedan',
            seats: 5,
            pricePerDay: 35,
            image: 'amaze-2024-exterior-left-front-three-quarter.webp',
            features: ['Electric', 'Autopilot', 'Touchscreen']
        },
        {
            id: 8,
            make: 'TOYOTA',
            model: 'FORTUNER',
            year: 2023,
            type: 'suv',
            seats: 8,
            pricePerDay: 100,
            image: 'images.jpeg',
            features: ['Electric', 'Autopilot', 'Touchscreen']
        },
        {
            id: 9,
            make: 'TOYOTA',
            model: 'INNOVA CRYSTA',
            year: 2023,
            type: 'suv',
            seats: 8,
            pricePerDay: 75,
            image: 'hq720.jpg',
            features: ['Electric', 'Autopilot', 'Touchscreen']
        },
        {
            id: 10,
            make: 'TOYOTA',
            model: 'C+pod EV',
            year: 2024,
            type: 'luxury',
            seats: 2,
            pricePerDay: 20,
            image: 'Toyota_C+pod_G_(ZAZ-RMV12-AGDQS)_front.jpg',
            features: ['Electric', 'Autopilot', 'Touchscreen']
        },
        {
            id: 11,
            make: 'Mahindra',
            model: 'scorpio',
            year: 2022,
            type: 'SUV',
            seats: 7,
            pricePerDay: 60,
            image: 'mahindra-scorpio-left-front-three-quarter2.webp',
            features: ['Electric', 'Autopilot', 'Touchscreen']
        },
        {
            id: 12,
            make: 'Maruti suzuki ',
            model: 'Ertiga',
            year: 2021,
            type: 'SUV',
            seats: 7,
            pricePerDay: 40,
            image: 'Maruti-Suzuki-XL6-Ertiga-MPV-7-seater-upgrade-3.jpeg',
            features: ['Electric', 'Autopilot', 'Touchscreen']
        },



    ];
    
    // DOM elements
    const carsContainer = document.getElementById('carsContainer');
    const typeFilter = document.getElementById('typeFilter');
    const priceFilter = document.getElementById('priceFilter');
    const seatsFilter = document.getElementById('seatsFilter');
    const bookingForm = document.getElementById('booking');
    const reservationForm = document.getElementById('reservationForm');
    const selectedCarInput = document.getElementById('selectedCar');
    const cancelBookingBtn = document.getElementById('cancelBooking');
    const submitBtn = document.getElementById('submitBtn');
    const btnText = submitBtn.querySelector('.btn-text');
    const loadingSpinner = submitBtn.querySelector('.loading-spinner');
    
    // Display cars with filters
    function displayCars() {
        const selectedType = typeFilter.value;
        const selectedPrice = priceFilter.value;
        const selectedSeats = seatsFilter.value;
        
        const filteredCars = cars.filter(car => {
            const typeMatch = selectedType === 'all' || car.type === selectedType;
            const priceMatch = selectedPrice === 'all' || car.pricePerDay <= parseInt(selectedPrice);
            const seatsMatch = selectedSeats === 'all' || 
                             (selectedSeats === '7' ? car.seats >= 7 : car.seats === parseInt(selectedSeats));
            
            return typeMatch && priceMatch && seatsMatch;
        });
        
        carsContainer.innerHTML = '';
        
        if (filteredCars.length === 0) {
            carsContainer.innerHTML = `
                <div class="no-cars">
                    <i class="fas fa-car-crash"></i>
                    <h3>No vehicles match your filters</h3>
                    <p>Try adjusting your search criteria</p>
                </div>
            `;
            return;
        }
        
        filteredCars.forEach(car => {
            const carCard = document.createElement('div');
            carCard.className = 'car-card';
            carCard.innerHTML = `
                <div class="car-image">
                    <img src="${car.image}" alt="${car.make} ${car.model}">
                </div>
                <div class="car-info">
                    <h3>${car.make} ${car.model}</h3>
                    <div class="car-meta">
                        <span><i class="fas fa-calendar-alt"></i> ${car.year}</span>
                        <span><i class="fas fa-users"></i> ${car.seats} seats</span>
                    </div>
                    <span class="car-type">${car.type.charAt(0).toUpperCase() + car.type.slice(1)}</span>
                    <p class="car-price">$${car.pricePerDay} <span>/ day</span></p>
                    <button class="btn btn-primary rent-btn" data-car-id="${car.id}">
                        <i class="fas fa-car"></i> Rent Now
                    </button>
                </div>
            `;
            carsContainer.appendChild(carCard);
        });
        
        // Add event listeners to rent buttons
        document.querySelectorAll('.rent-btn').forEach(btn => {
            btn.addEventListener('click', function() {
                const carId = parseInt(this.getAttribute('data-car-id'));
                const car = cars.find(c => c.id === carId);
                
                // Set selected car
                selectedCarInput.value = `${car.make} ${car.model} (${car.year})`;
                
                // Set default dates (today and tomorrow)
                const today = new Date();
                const tomorrow = new Date(today);
                tomorrow.setDate(tomorrow.getDate() + 1);
                
                document.getElementById('pickupDate').valueAsDate = today;
                document.getElementById('returnDate').valueAsDate = tomorrow;
                
                // Clear other fields
                document.getElementById('customerName').value = '';
                document.getElementById('customerEmail').value = '';
                document.getElementById('customerPhone').value = '';
                
                // Show booking form
                document.querySelector('.car-listings').classList.add('hidden');
                bookingForm.classList.remove('hidden');
                
                // Scroll to form
                window.scrollTo({
                    top: bookingForm.offsetTop - 80,
                    behavior: 'smooth'
                });
            });
        });
    }
    
    // Filter cars when filters change
    typeFilter.addEventListener('change', displayCars);
    priceFilter.addEventListener('change', displayCars);
    seatsFilter.addEventListener('change', displayCars);
    
    // Cancel booking
    cancelBookingBtn.addEventListener('click', function() {
        bookingForm.classList.add('hidden');
        document.querySelector('.car-listings').classList.remove('hidden');
    });
    
    // Handle reservation form submission
    reservationForm.addEventListener('submit', async function(e) {
        e.preventDefault();
        
        // Get form values
        const pickupDate = new Date(document.getElementById('pickupDate').value);
        const returnDate = new Date(document.getElementById('returnDate').value);
        const customerName = document.getElementById('customerName').value.trim();
        const customerEmail = document.getElementById('customerEmail').value.trim();
        const customerPhone = document.getElementById('customerPhone').value.trim();
        const carDescription = document.getElementById('selectedCar').value;
        
        // Validate form
        if (returnDate <= pickupDate) {
            showAlert('Return date must be after pickup date', 'error');
            return;
        }
        
        if (!customerName || !customerEmail || !customerPhone) {
            showAlert('Please fill in all required fields', 'error');
            return;
        }
        
        if (!/^\w+([.-]?\w+)*@\w+([.-]?\w+)*(\.\w{2,3})+$/.test(customerEmail)) {
            showAlert('Please enter a valid email address', 'error');
            return;
        }
        
        // Show loading state
        btnText.classList.add('hidden');
        loadingSpinner.classList.remove('hidden');
        submitBtn.disabled = true;
        
        try {
            // In a real app, this would be a fetch to your backend
            const response = await mockApiRequest({
                carDescription,
                pickupDate,
                returnDate,
                customerName,
                customerEmail,
                customerPhone
            });
            
            if (response.success) {
                // Show success message
                showAlert('Your booking has been confirmed!', 'success');
                
                // Display confirmation details
                displayConfirmationDetails({
                    car: carDescription,
                    pickupDate: pickupDate.toDateString(),
                    returnDate: returnDate.toDateString(),
                    duration: response.duration,
                    totalPrice: response.totalPrice,
                    customerName,
                    customerEmail,
                    confirmationNumber: response.confirmationNumber
                });
                
                // Reset form
                reservationForm.reset();
                bookingForm.classList.add('hidden');
                
                // Show confirmation modal
                openModal(confirmationModal);
            } else {
                showAlert(response.message || 'Booking failed. Please try again.', 'error');
            }
        } catch (error) {
            console.error('Booking error:', error);
            showAlert('An error occurred. Please try again.', 'error');
        } finally {
            // Reset button state
            btnText.classList.remove('hidden');
            loadingSpinner.classList.add('hidden');
            submitBtn.disabled = false;
        }
    });
    
    // Mock API function (replace with actual fetch)
    function mockApiRequest(bookingData) {
        return new Promise((resolve) => {
            setTimeout(() => {
                const days = Math.ceil(
                    (new Date(bookingData.returnDate) - new Date(bookingData.pickupDate)) / 
                    (1000 * 60 * 60 * 24)
                );
                const carPrice = cars.find(c => 
                    `${c.make} ${c.model} (${c.year})` === bookingData.carDescription
                ).pricePerDay;
                const totalPrice = days * carPrice;
                
                resolve({
                    success: true,
                    duration: days,
                    totalPrice,
                    confirmationNumber: generateConfirmationNumber()
                });
            }, 1500); // Simulate network delay
        });
    }
    
    function generateConfirmationNumber() {
        return Math.random().toString(36).substr(2, 8).toUpperCase();
    }
    
    function displayConfirmationDetails(details) {
        const confirmationDetails = document.getElementById('confirmationDetails');
        confirmationDetails.innerHTML = `
            <div class="confirmation-icon">
                <i class="fas fa-check-circle"></i>
            </div>
            <h3>Thank you for your booking!</h3>
            <div class="confirmation-details">
                <p><strong>Confirmation #:</strong> ${details.confirmationNumber}</p>
                <p><strong>Vehicle:</strong> ${details.car}</p>
                <p><strong>Pickup Date:</strong> ${details.pickupDate}</p>
                <p><strong>Return Date:</strong> ${details.returnDate}</p>
                <p><strong>Duration:</strong> ${details.duration} days</p>
                <p><strong>Total Price:</strong> $${details.totalPrice.toFixed(2)}</p>
                <p><strong>Customer:</strong> ${details.customerName}</p>
            </div>
            <div class="confirmation-note">
                <p>A confirmation email has been sent to ${details.customerEmail}</p>
                <p>Please bring your driver's license and this confirmation number when picking up your vehicle.</p>
            </div>
        `;
    }
    
    // Print confirmation
    document.getElementById('printConfirmation').addEventListener('click', function() {
        window.print();
    });
    
    // Initialize the page
    displayCars();
});