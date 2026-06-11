// script.js - Updated for combined ACS and IRS PageRank data

// create map *without* its default zoom buttons
let map = L.map("map", {
  center: [37.8, -96],
  zoom:   4,
  zoomControl: false
});

L.control.zoom({ position: "topright" }).addTo(map);

let geoLayer;

L.tileLayer("https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png", {
  attribution: "Map data © OpenStreetMap contributors"
}).addTo(map);

const FILES = {
  county_irs: "data/irs_county_pagerank.geojson",
  axel_national: "data/axel_national.geojson",
  metro_irs: "data/irs_cbsa_pagerank.geojson",
  acs_cbsa_rolling5: "data/acs_cbsa_rolling5_pagerank.geojson" 
};


let maxRank = 100;

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
  "educ_SomeCollege": "Some College",
  "educ_BA": "Bachelor's",
  "educ_Grad": "Post-Bachelor's"
};

function setSourceLabel(src) {
  const el = document.getElementById("source-text");
  if (el) el.textContent = src;
}

function getSelectedColumn() {
  const type = document.getElementById("filter-type-select")?.value;
  const geography = document.querySelector('input[name="geography"]:checked')?.value;
  const acsYear = document.getElementById("acs-year-select")?.value;

  if (geography === "neighborhood") {
    return `rank`;
  }

  if (geography === "county") {
    const val = document.getElementById("year-select").value;
    return `irs_county_rank_${val}`;
  }

  if (geography === "metro") {
    const source = document.querySelector('input[name="metro-source"]:checked')?.value;
    const isACS = (source === "acs");
    const hasYear = isACS && acsYear && acsYear !== "All";
    const suffix = hasYear ? `_${acsYear}` : "";

    if (source === "irs") {
      const val = document.getElementById("yeartwo-select").value;
      return `irs_metro_rank_${val}`;
    }

    if (type === "race") {
      return `rank_race_${document.getElementById("race-select").value}${suffix}`;
    }
    if (type === "age") {
      const val = document.getElementById("age-select").value;
      return `rank_age_${val.replace("+", "plus").replace("-", "_")}${suffix}`;
    }
    if (type === "education") {
      return `rank_educ_${document.getElementById("educ-select").value}${suffix}`;
    }
    if (type === "industry") {
      const code = document.getElementById("industry-input").value;
      return `rank_industry_${code}${suffix}`;
    }
    if (type === "children") {
      return `rank_kids_flow_${document.getElementById("children-select").value}${suffix}`;
    }
    if (type === "workstatus") {
      return `rank_retired_flow_${document.getElementById("workstatus-select").value}${suffix}`;
    }
    if (type === "tenure") {
      return `rank_tenure_${document.getElementById("tenure-select").value}${suffix}`;
    }
    
    return `rank_total_flowPER${suffix}`;
  }

  // fallback
  return "rank_total_flowPER";
}


function loadLayer(column, geography) {
  let url, labelField, sourceName;

  if (geography === "county") {
    url        = FILES.county_irs;
    labelField = "NAMELSAD";
    sourceName = "IRS county-level migration counts, 1991-2022";
  } else if (geography === "neighborhood") {
  url = FILES.axel_national;
  labelField = "district_id";  // or "name" if you added a label column
  sourceName = "DataAxel neighborhood migration data, 2019–2023 (National)";
  } else {
    const source = document.querySelector('input[name="metro-source"]:checked')?.value;
    const acsYear = document.getElementById("acs-year-select")?.value;
    const useRolling = source === "acs" && acsYear && acsYear !== "All";
  
    if (source === "irs") {
      url = FILES.metro_irs;
      sourceName = "IRS migration counts, 1991-2022, aggregated to the metro level";
    } else {
      url = FILES.acs_cbsa_rolling5;
      sourceName = `ACS microdata, 2005-2023`;
    } 
  
    labelField = "NAME";
  }


  setSourceLabel(sourceName);

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

  const p = 1 - (d - 1) / (maxRank - 1);

  if (p <= 0.50) return "#f46d43";
  if (p <= 0.75) return "#fdae61";
  if (p <= 0.80) return "#fee08b";
  if (p <= 0.90) return "#d9ef8b";
  if (p <= 0.95) return "#a6d96a";
  if (p <= 0.99) return "#66bd63";
  return "#1a9850";
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
    children: document.getElementById("children-wrapper"),
    tenure: document.getElementById("tenure-wrapper"),
    workstatus: document.getElementById("workstatus-wrapper"),
    year: document.getElementById("yeartwo-wrapper")
  };

  Object.keys(wrappers).forEach(key => {
    if (wrappers[key]) {
      wrappers[key].classList.toggle("hidden", key !== val);
    }
  });

  const metroSource = document.querySelector('input[name="metro-source"]:checked')?.value;
  document.getElementById("acs-filter-options")?.classList.toggle("hidden", metroSource !== "acs");
  document.getElementById("irs-year-wrapper")?.classList.toggle("hidden", metroSource !== "irs");
}

function setupEventListeners() {
  document.querySelectorAll('input[name="geography"]').forEach(radio => {
    radio.addEventListener("change", () => {
      updateFilterVisibility();
      refreshMap();
    });
  });

  document.getElementById("filter-type-select").addEventListener("change", () => {
    updateFilterVisibility();
    refreshMap();
  });

    [
    "race-select",
    "age-select",
    "educ-select",
    "year-select",
    "yeartwo-select",
    "acs-year-select",
    "children-select",
    "tenure-select",
    "workstatus-select"
  ].forEach(id => {
    const el = document.getElementById(id);
    if (el) el.addEventListener("change", refreshMap);
  });


  const ind = document.getElementById("industry-input");
  if (ind) ind.addEventListener("change", refreshMap);

  document.querySelectorAll('input[name="metro-source"]').forEach(radio => {
    radio.addEventListener("change", () => {
      updateFilterVisibility();
      refreshMap();
    });
  });

  const download = document.getElementById("download");
    if (download) {
    download.addEventListener("click", () => {
      const geography = document.querySelector('input[name="geography"]:checked')?.value;
      let path = null;

      if (geography === "neighborhood") {
        path = FILES.axel_national;
      } else if (geography === "county") {
        path = FILES.county_irs;
      } else {
        const source = document.querySelector('input[name="metro-source"]:checked')?.value;
        const acsYear = document.getElementById("acs-year-select")?.value;
        const useRolling = source === "acs" && acsYear && acsYear !== "All";
        path = (source === "irs")
          ? FILES.metro_irs
          : FILES.acs_cbsa_rolling5;
      }

      if (!path) {
        console.error("Could not determine CSV to download.");
        return;
      }

      const csvPath = path.replace(".geojson", ".csv");
      const anchor = document.createElement("a");
      anchor.href = csvPath;
      anchor.download = csvPath.split("/").pop();
      anchor.click();
    });
  } 
}

// Moved back outside the download block
document.addEventListener("DOMContentLoaded", () => {
  setupEventListeners();     // wire up your “geography” & “filter” controls
  updateFilterVisibility();  // show the right controls for the default radio
  refreshMap();              // draw that first layer
});










