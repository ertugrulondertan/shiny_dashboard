"use client"

import { ArrowUpRight } from "lucide-react"

const projects = [
  {
    title: "Savanna Grill",
    category: "Restaurant Branding",
    description: "Complete brand identity and social media presence",
    image: "/placeholder.svg?height=400&width=600",
  },
  {
    title: "Minetec Corporation",
    category: "Corporate Content",
    description: "Professional video production and photography",
    image: "/placeholder.svg?height=400&width=600",
  },
  {
    title: "Urban Threads",
    category: "Product Launch",
    description: "Social media campaign with 2M+ reach",
    image: "/placeholder.svg?height=400&width=600",
  },
  {
    title: "Apex Digital",
    category: "Website Design",
    description: "Modern website with 40% conversion increase",
    image: "/placeholder.svg?height=400&width=600",
  },
  {
    title: "Fresh Market",
    category: "Content Production",
    description: "Monthly content strategy and execution",
    image: "/placeholder.svg?height=400&width=600",
  },
  {
    title: "Nova Studio",
    category: "Visual Identity",
    description: "Complete rebrand and style guide",
    image: "/placeholder.svg?height=400&width=600",
  },
]

export function TNPortfolio() {
  return (
    <section id="work" className="py-24 md:py-32">
      <div className="max-w-6xl mx-auto px-6">
        {/* Section Header */}
        <div className="text-center mb-16">
          <span className="inline-block px-4 py-1.5 rounded-full border border-border/50 bg-card/50 text-sm text-muted-foreground mb-4">
            Our Work
          </span>
          <h2 className="text-3xl md:text-4xl lg:text-5xl font-bold text-foreground mb-4 text-balance">
            Featured Projects
          </h2>
          <p className="text-lg text-muted-foreground max-w-2xl mx-auto text-pretty">
            A selection of recent work that showcases our creative capabilities and results.
          </p>
        </div>

        {/* Projects Grid */}
        <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6">
          {projects.map((project, index) => (
            <div
              key={index}
              className="group relative rounded-2xl overflow-hidden border border-border/50 bg-card/30 hover:border-border transition-all duration-300 cursor-pointer"
            >
              {/* Image */}
              <div className="aspect-[4/3] bg-muted/50 relative overflow-hidden">
                <div className="absolute inset-0 bg-gradient-to-br from-primary/10 to-transparent" />
                <div className="absolute inset-0 flex items-center justify-center">
                  <span className="text-muted-foreground/50 text-sm">Project Image</span>
                </div>
                
                {/* Hover overlay */}
                <div className="absolute inset-0 bg-background/80 opacity-0 group-hover:opacity-100 transition-opacity flex items-center justify-center">
                  <div className="w-12 h-12 rounded-full bg-primary flex items-center justify-center">
                    <ArrowUpRight className="w-5 h-5 text-primary-foreground" />
                  </div>
                </div>
              </div>

              {/* Content */}
              <div className="p-6">
                <span className="text-xs uppercase tracking-wider text-primary font-medium">
                  {project.category}
                </span>
                <h3 className="text-lg font-semibold text-foreground mt-2 mb-1">
                  {project.title}
                </h3>
                <p className="text-sm text-muted-foreground">
                  {project.description}
                </p>
              </div>
            </div>
          ))}
        </div>
      </div>
    </section>
  )
}
