// Persistent login functionality using localStorage

$(document).ready(function() {
  
  // Check if localStorage is available
  function isLocalStorageAvailable() {
    try {
      var test = 'localStorage-test';
      localStorage.setItem(test, test);
      localStorage.removeItem(test);
      return true;
    } catch(e) {
      console.warn("localStorage is not available:", e);
      return false;
    }
  }
  
  // Handler to store user data in localStorage
  Shiny.addCustomMessageHandler("storeUser", function(userData) {
    if (!isLocalStorageAvailable()) {
      console.warn("Cannot store user data: localStorage not available");
      return;
    }
    
    try {
      console.log("userData type:", typeof userData);
      console.log("userData value:", userData);
      
      // Check if userData is already an object or a string
      var dataToStore;
      if (typeof userData === 'string') {
        dataToStore = userData;
      } else {
        // If it's an object, stringify it
        dataToStore = JSON.stringify(userData);
      }
      
      localStorage.setItem("gameOnTickerUser", dataToStore);
      console.log("Stored user data:", dataToStore);
    } catch (e) {
      console.warn("Failed to store user data:", e);
    }
  });
  
  // Handler to clear stored user data
  Shiny.addCustomMessageHandler("clearStoredUser", function(message) {
    if (!isLocalStorageAvailable()) {
      return;
    }
    
    try {
      localStorage.removeItem("gameOnTickerUser");
      console.log("Cleared user data from localStorage");
    } catch (e) {
      console.warn("Failed to clear user data:", e);
    }
  });
  
  // Handler to retrieve stored user data
  Shiny.addCustomMessageHandler("getStoredUser", function(message) {
    if (!isLocalStorageAvailable()) {
      console.warn("Cannot retrieve user data: localStorage not available");
      Shiny.setInputValue("login_1-storedUser", null, {priority: "event"});
      return;
    }
    
    try {
      var storedUser = localStorage.getItem("gameOnTickerUser");
      console.log("Retrieved stored user:", storedUser);
      
      // Send back to Shiny - ensure we send a value even if null
      Shiny.setInputValue("login_1-storedUser", storedUser || "", {priority: "event"});
    } catch (e) {
      console.warn("Failed to retrieve user data:", e);
      // Send empty string if retrieval fails
      Shiny.setInputValue("login_1-storedUser", "", {priority: "event"});
    }
  });
  
});
