"use client"

import { 
  Share2, 
  Camera, 
  Video, 
  Globe, 
  Palette, 
  TrendingUp,
  ArrowUpRight
} from "lucide-react"

const services = [
  {
    icon: Share2,
    title: "Social Media Management",
    description: "Strategic content planning, posting, and community management to grow your brand presence across all platforms.",
  },
  {
    icon: Camera,
    title: "Content Creation",
    description: "Compelling visual content that tells your story and connects with your audience on a deeper level.",
  },
  {
    icon: Video,
    title: "Photography & Videography",
    description: "Professional production that captures the essence of your brand through stunning visuals and motion.",
  },
  {
    icon: Globe,
    title: "Website Design",
    description: "Modern, responsive websites that convert visitors into customers with seamless user experiences.",
  },
  {
    icon: Palette,
    title: "Branding & Strategy",
    description: "Comprehensive brand identities and strategic direction that set you apart from the competition.",
  },
  {
    icon: TrendingUp,
    title: "Paid Advertising",
    description: "Data-driven campaigns that maximize ROI and drive measurable results for your business.",
  },
]

export function TNServices() {
  return (
    <section id="services" className="py-24 md:py-32">
      <div className="max-w-6xl mx-auto px-6">
        {/* Section Header */}
        <div className="text-center mb-16">
          <span className="inline-block px-4 py-1.5 rounded-full border border-border/50 bg-card/50 text-sm text-muted-foreground mb-4">
            What We Do
          </span>
          <h2 className="text-3xl md:text-4xl lg:text-5xl font-bold text-foreground mb-4 text-balance">
            Services Built for Growth
          </h2>
          <p className="text-lg text-muted-foreground max-w-2xl mx-auto text-pretty">
            From strategy to execution, we provide everything your brand needs to stand out and succeed.
          </p>
        </div>

        {/* Services Grid */}
        <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6">
          {services.map((service, index) => (
            <div
              key={index}
              className="group relative p-6 rounded-2xl border border-border/50 bg-card/30 backdrop-blur-sm hover:bg-card/60 hover:border-border transition-all duration-300 cursor-pointer"
            >
              {/* Icon */}
              <div className="w-12 h-12 rounded-xl bg-primary/10 flex items-center justify-center mb-4 group-hover:bg-primary/20 transition-colors">
                <service.icon className="w-6 h-6 text-primary" />
              </div>

              {/* Content */}
              <h3 className="text-lg font-semibold text-foreground mb-2 flex items-center gap-2">
                {service.title}
                <ArrowUpRight className="w-4 h-4 opacity-0 -translate-x-2 group-hover:opacity-100 group-hover:translate-x-0 transition-all text-primary" />
              </h3>
              <p className="text-muted-foreground text-sm leading-relaxed">
                {service.description}
              </p>

              {/* Hover glow effect */}
              <div className="absolute inset-0 rounded-2xl bg-primary/5 opacity-0 group-hover:opacity-100 transition-opacity pointer-events-none" />
            </div>
          ))}
        </div>
      </div>
    </section>
  )
}
