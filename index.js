// Set Header Dynamic Font
const header = document.querySelector( ".head" );
header.style.fontSize = clampBuilder( 320, 1920, 1, 2 );


function clampBuilder( minWidthPx, maxWidthPx, minFontSize, maxFontSize ) {
        const root = document.querySelector( "html" );
        const pixelsPerRem = Number( getComputedStyle( root ).fontSize.slice( 0,-2 ) );
      
        const minWidth = minWidthPx / pixelsPerRem;
        const maxWidth = maxWidthPx / pixelsPerRem;
      
        const slope = ( maxFontSize - minFontSize ) / ( maxWidth - minWidth );
        const yAxisIntersection = -minWidth * slope + minFontSize
      
        return `clamp( ${ minFontSize }rem, ${ yAxisIntersection }rem + ${ slope * 100 }vw, ${ maxFontSize }rem )`;
}

// Find Head Height

const headHeight = header.offsetHeight;

// Find Total Height

const totalHeight = document.body.offsetHeight;

// Select Sections

let sections = document.querySelectorAll( "div[class^='sec']" );

gsap.fromTo('.head', {background: '#16222A', color: 'white'}, {background: 'black', color:'white' ,duration: 5, delay: 1, ease: 'power2.inOut', repeat: -1, yoyo: true});

gsap.registerPlugin(ScrollTrigger);

let panels = gsap.utils.toArray('.content > div');
let tops = panels.map(panel => ScrollTrigger.create({trigger: panel, start: "top top"}));

// Split Text into Spans

function splitTextIntoSpans(container) {
    const text = container.textContent;
    const spans = Array.from(text).map(char => {
        const span = document.createElement('span');
        span.textContent = char;
        span.classList.add('animated-text');
        return span;
    });

    // Clear the container and append the spans
    container.innerHTML = '';
    spans.forEach(span => container.appendChild(span));
}

panels.forEach((panel, i) => {

    ScrollTrigger.create({
      trigger: panel,
      start: "bottom bottom", // if it's shorter than the viewport, we prefer to pin it at the top
      pin: true, 
      pinSpacing: false,
      onEnter: () => {
          let back = panel.querySelector('.back')
          let work = panel.querySelector('.work')
          let workNodes = work.querySelectorAll('*')
          let tl = gsap.timeline();
          tl.to(back, {opacity: 0, duration: 3, delay:1 ,ease: 'power2.inOut'})
          tl.set(back, {display: 'none'}, '>')
          tl.to(work, {opacity: 1, duration: 2, ease: 'power2.inOut'},'<')
          tl.to(workNodes,{opacity: 1,x:0,y:0,duration: 1.5, ease: 'power2.inOut', stagger: 0.2},'<')        
        }
    });
  });
//  ScrollTrigger.create({
//     snap: {
//       snapTo: (progress, self) => {
//         let panelStarts = tops.map(st => st.start), // an Array of all the starting scroll positions. We do this on each scroll to make sure it's totally responsive. Starting positions may change when the user resizes the viewport
//             snapScroll = gsap.utils.snap(panelStarts, self.scroll()); // find the closest one
//         return gsap.utils.normalize(0, ScrollTrigger.maxScroll(window), snapScroll); // snapping requires a progress value, so convert the scroll position into a normalized progress value between 0 and 1
//       }
//     }
//     });

  let work = document.querySelectorAll('.work');

  console.log(work);

  work.forEach((work, i) => {
    work.style.height = `${window.innerHeight - headHeight}px`;
    work.style.marginTop = `${headHeight}px`;
    }
    );


gsap.registerPlugin(Observer) 
