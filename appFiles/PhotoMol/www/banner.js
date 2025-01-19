document.addEventListener('DOMContentLoaded', function() {
  var banners = document.querySelectorAll('[data-vf-js-banner]');

  banners.forEach(function(banner) {
    var closeButton = banner.querySelector('[data-vf-js-banner-close]');
    var cookieName = banner.getAttribute('data-vf-js-banner-cookie-name');
    var cookieVersion = banner.getAttribute('data-vf-js-banner-cookie-version');
    var cookieDuration = banner.getAttribute('data-vf-js-banner-cookie-duration') || 30; // Default to 30 days if not specified

    // Check if the cookie is already set
    if (document.cookie.split(';').some((item) => item.trim().startsWith(cookieName + '=' + cookieVersion))) {
      banner.style.display = 'none';
    }

    closeButton.addEventListener('click', function() {
      // Set the cookie
      document.cookie = cookieName + '=' + cookieVersion + '; path=/; max-age=' + (cookieDuration * 24 * 60 * 60);
      // Hide the banner
      banner.style.display = 'none';
    });
  });
});