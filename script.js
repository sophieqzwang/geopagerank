
// script.js - Updated for combined ACS and IRS PageRank data

// create map *without* its default zoom buttons
let map = L.map("map", {
  center: [37.8, -96],
  zoom:   4,
  zoomControl: false        // turn off default (topleft) control
});

// add the zoom buttons back on the right
L.control.zoom({ position: "topright" }).addTo(map);

let geoLayer;

L.tileLayer("https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png", {
  attribution: "Map data © OpenStreetMap contributors"
}).addTo(map);

const FILES = {
  county_irs: "data/irs_county_pagerank_combined.geojson",
  axel_Chicago: "data/axel_Chicago.geojson",
  axel_Boston: "data/axel_Boston.geojson",  
  metro_irs: "data/irs_pagerank_combined.geojson",
  metro_acs: "data/acs_pagerank_combined.geojson"
};

let maxRank = 100;  // default, will be updated per dataset

const LABEL_MAP = {
  "total_flowHH": "Total Household Flow",
  "total_flowPER": "Total Person Flow",
  "race_Black": "Black",
  "race_White": "White",
  "race_Hispanic": "Hispanic",
  "age_25_35": "Age 25–35",
  "age_35_65": "Age 35–65",
  "age_65plus": "Age 65+",
  "educ_NoCollege": "No College",
  "educ_WithCollege": "With College"
};

function getSelectedColumn() {
  const type = document.getElementById("filter-type-select")?.value;
  const geography = document.querySelector('input[name="geography"]:checked')?.value;
  
  if (geography === "neighborhood") {
    return `rank`;
  }
  if (geography === "county") {
    const val = document.getElementById("year-select").value;
    return `irs_county_rank_${val}`;
  }
  if (geography !== "county" && type === "year") {
    const val = document.getElementById("yeartwo-select").value;
    return `irs_metro_rank_${val}`;
  }
  if (type === "race") {
    return `rank_race_${document.getElementById("race-select").value}`;
  }
  if (type === "age") {
    const val = document.getElementById("age-select").value;
    return `rank_age_${val.replace("+", "plus").replace("-", "_")}`;
  }
  if (type === "education") {
    return `rank_educ_${document.getElementById("educ-select").value}`;
  }
  if (type === "industry") {
    const code = document.getElementById("industry-input").value;
    return `rank_industry_${code}`;
  }

  return "rank_total_flowPER"; // fallback
}

function loadLayer(column, geography) {
  let url, labelField;

  if (geography === "county") {
    const key = "county_irs";  // Hardcode to IRS since no Source-select exists

    url = "/geopagerank/" + FILES[key];

    labelField = "NAMELSAD";
  } else if (geography === "neighborhood"){
      
    const city = document.getElementById("city-select").value;
    if (city === "Chicago") {
      url = "/geopagerank/" + FILES.axel_Chicago;
    } else if (city === "Boston") {
      url = "/geopagerank/" + FILES.axel_Boston;
    } else {
      console.error("Unknown city:", city);
      return;
    }

  labelField = "district_id"; // or use another property for popup labeling if needed

      
  } else {
    const type = document.getElementById("filter-type-select").value;
    url = "/geopagerank/" + ((type === "year" || type === "none") ? FILES.metro_irs : FILES.metro_acs);
    labelField = "NAME";
  }

  fetch(url)
    .then(res => res.json())
    .then(data => {
      if (!data.features) return;

      const filtered = data.features.filter(f => {
        const props = f.properties || {};
        return props[column] != null;
      });

      const values = filtered.map(f => f.properties[column]).filter(v => v != null && !isNaN(v));
      maxRank = Math.max(...values);

      if (geoLayer) map.removeLayer(geoLayer);

      geoLayer = L.geoJSON({ type: "FeatureCollection", features: filtered }, {
        style: feature => ({
          fillColor: getColor(feature.properties[column]),
          weight: 1,
          opacity: 1,
          color: 'white',
          dashArray: '3',
          fillOpacity: 0.8
        }),
        // --- inside loadLayer(...) -----------------------------------------------
        onEachFeature: (feature, layer) => {
          const name = feature.properties[labelField] || "Unnamed";
          const val  = feature.properties[column];
          const rankText = (val != null && !isNaN(val))
            ? `Rank: ${val} / ${maxRank}`
            : "Rank: N/A";
          layer.bindPopup(`<strong>${name}</strong><br>${rankText}`);

        }


      }).addTo(map);

    })
    .catch(err => {
      console.error("Failed to load GeoJSON:", err);
    });
}

function getColor(d) {
  if (d == null || isNaN(d) || !maxRank) return "#ccc";

  // Compute percentile: lower rank = better
  const p = 1 - (d - 1) / (maxRank - 1);

  if (p <= 0.50) return "#f46d43";
  if (p <= 0.75) return "#fdae61";
  if (p <= 0.80) return "#fee08b";
  if (p <= 0.90) return "#d9ef8b";
  if (p <= 0.95) return "#a6d96a";
  if (p <= 0.99) return "#66bd63";
  return "#1a9850";  // top ~1%
}



function refreshMap() {
  const geography = document.querySelector('input[name="geography"]:checked')?.value;
  const column = getSelectedColumn();
  console.log("Selected column:", column);
  if (column && geography) {
    loadLayer(column, geography);
  }
}

function updateFilterVisibility() {
  const val = document.getElementById("filter-type-select").value;
  const wrappers = {
    race: document.getElementById("race-wrapper"),
    age: document.getElementById("age-wrapper"),
    education: document.getElementById("educ-wrapper"),
    industry: document.getElementById("industry-wrapper"),
    year: document.getElementById("yeartwo-wrapper")
  };
  Object.keys(wrappers).forEach(key => {
    if (wrappers[key]) {
      wrappers[key].classList.toggle("hidden", key !== val);
    }
  });
}

function setupEventListeners() {
  // -- Geography radio buttons (Neighborhood / County / Metro)
  document.querySelectorAll('input[name="geography"]').forEach(radio => {
    radio.addEventListener("change", () => {
      updateFilterVisibility();
      refreshMap();               // redraw as soon as geography changes
    });
  });

  // -- Metro “Filter by” dropdown (total / race / age / … / year)
  document.getElementById("filter-type-select").addEventListener("change", () => {
    updateFilterVisibility();
    refreshMap();                 // redraw as soon as filter type changes
  });

  // -- All subtype selectors that can change the column we need
  //      • added "year-select" so County-year changes fire refreshMap()
  ["race-select", "age-select", "educ-select", "year-select", "yeartwo-select"].forEach(id => {
    const el = document.getElementById(id);
    if (el) el.addEventListener("change", refreshMap);
  });

  // -- Industry dropdown (1-digit NAICS)
  const ind = document.getElementById("industry-input");
  if (ind) ind.addEventListener("change", refreshMap);

// --- Download-button handler (new) ------------------------------------
const download = document.getElementById("download");
if (download) {
  download.addEventListener("click", () => {
    const geography = document.querySelector('input[name="geography"]:checked')?.value;
    let   path      = null;

    if (geography === "neighborhood") {
      const city = document.getElementById("city-select").value;
      path = (city === "Chicago") ? FILES.axel_Chicago
           : (city === "Boston")  ? FILES.axel_Boston
           : null;
    } else if (geography === "county") {
      path = FILES.county_irs;
    } else {                         // metro
      const type = document.getElementById("filter-type-select").value;
      path = (type === "year" || type === "none")
           ? FILES.metro_irs        // IRS metro file for total or year
           : FILES.metro_acs;       // ACS metro file for race/age/educ/industry
    }

    if (!path) {                     // defensive guard
      console.error("Could not determine CSV to download.");
      return;
    }

    const csvPath  = path.replace(".geojson", ".csv");
    const anchor   = document.createElement("a");
    anchor.href    = csvPath;
    anchor.download = csvPath.split("/").pop();  // just the file name
    anchor.click();
  });
}

}


document.addEventListener("DOMContentLoaded", () => {
  setupEventListeners();
  updateFilterVisibility();
  refreshMap();

  const cityBounds = {
    Chicago: [[41.20, -88.40], [42.40, -87.00]],  // zoomed out more
    Boston:  [[41.80, -71.70], [42.70, -70.60]]   // zoomed out more
  };


  const citySelect = document.getElementById("city-select");
  const geographyRadios = document.querySelectorAll('input[name="geography"]');

  // Recenter when city is changed
  if (citySelect) {
    citySelect.addEventListener("change", () => {
      const city = citySelect.value;
      if (cityBounds[city]) {
        map.fitBounds(cityBounds[city]);
      }
      refreshMap();
    });
  }

  // Recenter when "Neighborhood" geography is selected
  geographyRadios.forEach(radio => {
    radio.addEventListener("change", () => {
      if (radio.checked && radio.value === "neighborhood") {
        const city = citySelect?.value;
        if (cityBounds[city]) {
          map.fitBounds(cityBounds[city]);
        }
      }
    });
  });
});








