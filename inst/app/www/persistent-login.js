// Persistent login functionality using localStorage

$(document).ready(function() {
  
  // Handler to store user data in localStorage
  Shiny.addCustomMessageHandler("storeUser", function(userData) {
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
    try {
      localStorage.removeItem("gameOnTickerUser");
    } catch (e) {
      console.warn("Failed to clear user data:", e);
    }
  });
  
  // Handler to retrieve stored user data
  Shiny.addCustomMessageHandler("getStoredUser", function(message) {
    try {
      var storedUser = localStorage.getItem("gameOnTickerUser");
      console.log("Retrieved stored user:", storedUser);
      // Send back to Shiny
      Shiny.setInputValue("login_1-storedUser", storedUser, {priority: "event"});
    } catch (e) {
      console.warn("Failed to retrieve user data:", e);
      // Send null if retrieval fails
      Shiny.setInputValue("login_1-storedUser", null, {priority: "event"});
    }
  });
  
});
